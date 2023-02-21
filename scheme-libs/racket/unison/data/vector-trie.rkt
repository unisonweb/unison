#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/fixnum
         racket/sequence
         racket/unsafe/ops)

(provide NODE-BITS
         NODE-CAPACITY
         NODE-INDEX-MASK

         vector-trie?
         empty-vector-trie
         (contract-out
          [vector-trie-length (-> vector-trie? exact-nonnegative-integer?)]
          [vector-trie-empty? (-> vector-trie? boolean?)]

          [vector-trie-ref (-> vector-trie? exact-nonnegative-integer? any/c)]
          [vector-trie-set (-> vector-trie? exact-nonnegative-integer? any/c vector-trie?)]
          [vector-trie-add-first (-> vector-trie? any/c vector-trie?)]
          [vector-trie-add-last (-> vector-trie? any/c vector-trie?)]
          [vector-trie-drop-first (-> (and/c vector-trie? (not/c vector-trie-empty?)) vector-trie?)]
          [vector-trie-drop-last (-> (and/c vector-trie? (not/c vector-trie-empty?)) vector-trie?)]

          [vector-trie-first (-> (and/c vector-trie? (not/c vector-trie-empty?)) any/c)]
          [vector-trie-last (-> (and/c vector-trie? (not/c vector-trie-empty?)) any/c)]
          [vector-trie-pop-first (-> (and/c vector-trie? (not/c vector-trie-empty?)) (values vector-trie? any/c))]
          [vector-trie-pop-last (-> (and/c vector-trie? (not/c vector-trie-empty?)) (values vector-trie? any/c))]
          [vector-trie-update (-> vector-trie? exact-nonnegative-integer? (-> any/c any/c) vector-trie?)]
          [vector-trie-append (-> vector-trie? vector-trie? vector-trie?)]
          [vector-trie-drop-left (-> vector-trie? exact-nonnegative-integer? vector-trie?)]
          [vector-trie-drop-right (-> vector-trie? exact-nonnegative-integer? vector-trie?)])

         (rename-out [-in-vector-trie in-vector-trie]))

;; -----------------------------------------------------------------------------

(define NODE-BITS 5) ; 32-way branching
(define NODE-CAPACITY (expt 2 NODE-BITS))
(define NODE-INDEX-MASK (sub1 NODE-CAPACITY))

;; A vector trie is an integer-indexed, 32-way branching trie that
;; serves as the core for an efficient persistent vector data
;; structure. It is based on Clojure’s persistent vector structure,
;; which is described in detail in this series of blog posts:
;;
;;  <https://hypirion.com/musings/understanding-persistent-vector-pt-1>
;;
;; The `length` field contains the logical length of the trie (which
;; may be smaller than the current capacity). The `shift` field is #f
;; if `length` is 0, otherwise it is
;;
;;     (* (floor (log (sub1 length) BITS-PER-LEVEL)) BITS-PER-LEVEL)
;;
;; which is the number of bits that an index into the trie needs to be
;; right-shifted to obtain the index into the root node. The `root-node`
;; field itself is #f if `length` is 0, otherwise it is a vector that
;; serves as the root of the trie.
;;
;; An extension we make to the basic design is the `offset` field,
;; which allows new elements to be efficiently prepended. It contains
;; the number of unused slots at the start of the physical trie; its
;; value is added to the logical index for each get/set operation to
;; obtain a physical index.
(struct vector-trie (length offset shift root-node)
  #:transparent
  #:property prop:equal+hash
  (let ()
    (define (equal-proc vt-a vt-b recur)
      (and (= (vector-trie-length vt-a)
              (vector-trie-length vt-b))
           (for/and ([val-a (-in-vector-trie vt-a)]
                     [val-b (-in-vector-trie vt-b)])
             (recur val-a val-b))))

    (define (->fx/wraparound v)
      (if (fixnum? v)
          v
          (bitwise-and v (most-positive-fixnum))))

    (define ((hash-proc init) vt recur)
      (for/fold ([hc init])
                ([val (-in-vector-trie vt)])
        (fxxor (fx*/wraparound hc 31) (->fx/wraparound (recur val)))))

    (list equal-proc
          (hash-proc 255615927)
          (hash-proc 422602749))))

(define empty-vector-trie (vector-trie 0 0 #f #f))

;; -----------------------------------------------------------------------------
;; node operations

(define (make-node initialize-proc)
  (define node (make-vector NODE-CAPACITY #f))
  (initialize-proc node)
  (unsafe-vector*->immutable-vector! node))

;; Makes a new chain of nodes containing a single value at the first
;; index such that the resulting node chain has the depth expected for
;; the given shift.
(define (make-singleton-nodes/first shift val)
  (for/fold ([child val])
            ([i (in-inclusive-range 0 shift NODE-BITS)])
    (make-node
     (λ (node)
       (vector-set! node 0 child)))))

;; Like `make-singleton-nodes/first`, but the value is placed at the
;; last index.
(define (make-singleton-nodes/last shift val)
  (for/fold ([child val])
            ([i (in-inclusive-range 0 shift NODE-BITS)])
    (make-node
     (λ (node)
       (vector-set! node (sub1 NODE-CAPACITY) child)))))

(define (vector-trie-node-set/immediate node i v)
  (make-node
   (λ (new-node)
     (vector-copy! new-node 0 node 0 i)
     (vector-set! new-node i v)
     (vector-copy! new-node (add1 i) node (add1 i)))))

(define (extract-node-index i shift)
  (bitwise-and (arithmetic-shift i (- shift)) NODE-INDEX-MASK))

;; Performs a functional update of a vector trie node at the given index.
(define (vector-trie-node-set node shift i val)
  (let loop ([shift shift]
             [node node])
    (define node-i (extract-node-index i shift))
    (vector-trie-node-set/immediate
     node node-i
     (if (zero? shift)
         val
         (loop (- shift NODE-BITS)
               (vector-ref node node-i))))))

;; Like `vector-trie-node-set`, but handles the case where new nodes
;; need to be created by applying the given `make-singleton-nodes` function.
(define (vector-trie-node-insert node shift i val #:make-singleton make-singleton-nodes)
  (let loop ([shift shift]
             [node node])
    (define node-i (extract-node-index i shift))
    (vector-trie-node-set/immediate
     node node-i
     (cond
       [(zero? shift)
        val]
       [else
        (define child-node (vector-ref node node-i))
        (if child-node
            (loop (- shift NODE-BITS) child-node)
            (make-singleton-nodes (- shift NODE-BITS) val))]))))

;; Like (vector-trie-node-set node shift i #f), but handles deleting
;; empty child nodes. If `from-start?` is #f, then a child node is
;; deleted if the element to be removed is its first element, otherwise a
;; child node is deleted if the element to be removed is its last element.
(define (vector-trie-node-delete node shift i #:from-start? from-start?)
  (define end-i (if from-start? (bitwise-not i) i))
  (let loop ([shift shift]
             [node node])
    (define node-i (extract-node-index i shift))
    (vector-trie-node-set/immediate
     node node-i
     (if (or (zero? shift)
             (zero? (bitwise-bit-field end-i 0 shift)))
         #f
         (loop (- shift NODE-BITS)
               (vector-ref node node-i))))))

;; -----------------------------------------------------------------------------
;; core operations

(define (vector-trie-empty? vt)
  (zero? (vector-trie-length vt)))

(define (check-index-in-range who vt i)
  (unless (< i (vector-trie-length vt))
    (raise-range-error who "vector trie" "" i vt 0 (sub1 (vector-trie-length vt)))))

(define (vector-trie-ref vt i)
  (check-index-in-range 'vector-trie-ref vt i)
  (let ([i (+ (vector-trie-offset vt) i)])
    (let loop ([shift (vector-trie-shift vt)]
               [node (vector-trie-root-node vt)])
      (define node-i (extract-node-index i shift))
      (if (zero? shift)
          (vector-ref node node-i)
          (loop (- shift NODE-BITS)
                (vector-ref node node-i))))))

(define (vector-trie-set vt i val)
  (check-index-in-range 'vector-trie-ref vt i)
  (struct-copy
   vector-trie vt
   [root-node (vector-trie-node-set (vector-trie-root-node vt)
                                    (vector-trie-shift vt)
                                    (+ (vector-trie-offset vt) i)
                                    val)]))

(define (vector-trie-add-first vt val)
  (cond
    [(vector-trie-empty? vt)
     (vector-trie 1 (sub1 NODE-CAPACITY) 0 (make-node (λ (node) (vector-set! node (sub1 NODE-CAPACITY) val))))]

    [(zero? (vector-trie-offset vt))
     ;; Out of room, need to allocate a new root.
     (define shift (vector-trie-shift vt))
     (define new-shift (+ shift NODE-BITS))
     (struct-copy
      vector-trie vt
      [length (add1 (vector-trie-length vt))]
      [offset (sub1 (* (arithmetic-shift 1 new-shift) (sub1 NODE-CAPACITY)))]
      [shift new-shift]
      [root-node
       (make-node
        (λ (new-root)
          (vector-set! new-root (sub1 NODE-CAPACITY) (vector-trie-root-node vt))
          (vector-set! new-root (- NODE-CAPACITY 2) (make-singleton-nodes/last shift val))))])]

    [else
     ;; Still have space in the root, just do an insertion.
     (define prev-i (sub1 (vector-trie-offset vt)))
     (struct-copy
      vector-trie vt
      [length (add1 (vector-trie-length vt))]
      [offset prev-i]
      [root-node (vector-trie-node-insert
                  #:make-singleton make-singleton-nodes/last
                  (vector-trie-root-node vt)
                  (vector-trie-shift vt)
                  prev-i
                  val)])]))

(define (vector-trie-add-last vt val)
  (cond
    [(vector-trie-empty? vt)
     (vector-trie 1 0 0 (make-node (λ (node) (vector-set! node 0 val))))]

    [else
     (define next-i (+ (vector-trie-offset vt) (vector-trie-length vt)))
     (define shift (vector-trie-shift vt))
     (if (= next-i (arithmetic-shift 1 (+ shift NODE-BITS)))

         ;; Out of room, need to allocate a new root.
         (struct-copy
          vector-trie vt
          [length (add1 (vector-trie-length vt))]
          [shift (+ shift NODE-BITS)]
          [root-node
           (make-node
            (λ (new-root)
              (vector-set! new-root 0 (vector-trie-root-node vt))
              (vector-set! new-root 1 (make-singleton-nodes/first shift val))))])

         ;; Still have space in the root, just do an insertion.
         (struct-copy
          vector-trie vt
          [length (add1 (vector-trie-length vt))]
          [root-node (vector-trie-node-insert
                      #:make-singleton make-singleton-nodes/first
                      (vector-trie-root-node vt)
                      shift
                      next-i
                      val)]))]))

(define (vector-trie-drop-first vt)
  (cond
    [(= (vector-trie-length vt) 1)
     empty-vector-trie]
    [else
     (define new-length (sub1 (vector-trie-length vt)))
     (define first-i (vector-trie-offset vt))
     (define shift (vector-trie-shift vt))
     (if (and
          ;; Do we have a root node we can pop?
          (> shift 0)
          ;; Does the new trie fit entirely in a single child node?
          (<= new-length (arithmetic-shift 1 shift))
          ;; Is the element we’re deleting the last element of its containing child node?
          (zero? (bitwise-bit-field (bitwise-not first-i) 0 shift)))

         ;; Need to pop the root.
         (struct-copy
          vector-trie vt
          [length new-length]
          [offset 0]
          [shift (- shift NODE-BITS)]
          [root-node (vector-ref (vector-trie-root-node vt) (extract-node-index (sub1 first-i) shift))])

         ;; Can’t pop the root, just delete an element.
         (struct-copy
          vector-trie vt
          [length new-length]
          [offset (add1 first-i)]
          [root-node (vector-trie-node-delete (vector-trie-root-node vt) shift first-i #:from-start? #t)]))]))

(define (vector-trie-drop-last vt)
  (cond
    [(= (vector-trie-length vt) 1)
     empty-vector-trie]
    [else
     (define new-length (sub1 (vector-trie-length vt)))
     (define last-i (+ (vector-trie-offset vt) (sub1 (vector-trie-length vt))))
     (define shift (vector-trie-shift vt))
     (if (and
          ;; Do we have a root node we can pop?
          (> shift 0)
          ;; Does the new trie fit entirely in a single child node?
          (<= new-length (arithmetic-shift 1 shift))
          ;; Is the element we’re deleting the first element of its containing child node?
          (zero? (bitwise-bit-field last-i 0 shift)))

         ;; Need to pop the root.
         (struct-copy
          vector-trie vt
          [length new-length]
          [shift (- shift NODE-BITS)]
          [root-node (vector-ref (vector-trie-root-node vt) (extract-node-index (add1 last-i) shift))])

         ;; Can’t pop the root, just delete an element.
         (struct-copy
          vector-trie vt
          [length new-length]
          [root-node (vector-trie-node-delete (vector-trie-root-node vt) shift last-i #:from-start? #f)]))]))

;; -----------------------------------------------------------------------------
;; derived operations

(define (vector-trie-first vt)
  (vector-trie-ref vt 0))

(define (vector-trie-last vt)
  (vector-trie-ref vt (sub1 (vector-trie-length vt))))

;; Could be made more efficient by fusing the ref and set
;; operations to only traverse the trie once.
(define (vector-trie-update vt i f)
  (check-index-in-range 'vector-trie-update vt i)
  (vector-trie-set vt i (f (vector-trie-ref vt i))))

;; Could be made more efficient by fusing the ref and drop
;; operations to only traverse the trie once.
(define (vector-trie-pop-first vt)
  (values (vector-trie-drop-first vt)
          (vector-trie-first vt)))

;; Could be made more efficient by fusing the ref and drop
;; operations to only traverse the trie once.
(define (vector-trie-pop-last vt)
  (values (vector-trie-drop-last vt)
          (vector-trie-last vt)))

;; TODO: Could be made more efficient by directly walking the internal
;; structure, avoiding repeated traversals.
(define (in-vector-trie vt)
  (unless (vector-trie? vt)
    (raise-argument-error 'in-vector-trie "vector-trie?" vt))
  (sequence-map (λ (i) (vector-trie-ref vt i)) (in-range (vector-trie-length vt))))
(define-sequence-syntax -in-vector-trie
  (λ () #'in-vector-trie)
  (syntax-parser
    [[(x:id) (_ {~var vt-e (expr/c #'vector-trie?)})]
     #'[(x) (:do-in
             ([(vt vt-len) (let ([vt vt-e.c])
                             (values vt (vector-trie-length vt)))])
             (void)
             ([i 0])
             (< i vt-len)
             ([(x) (vector-trie-ref vt i)])
             #t
             #t
             [(add1 i)])]]
    [_ #f]))

;; FIXME: Currently very inefficient. Should be made a primitive
;; operation that does bulk copying into the nodes of the result trie
;; and avoids repeated traversal of the input trie.
(define (vector-trie-append vt-a vt-b)
  (if (<= (vector-trie-length vt-a) (vector-trie-length vt-b))
      (for/fold ([vt vt-a])
                ([val (-in-vector-trie vt-b)])
        (vector-trie-add-last vt val))
      (for/fold ([vt vt-b])
                ([i (in-inclusive-range (sub1 (vector-trie-length vt-a)) 0 -1)])
        (vector-trie-add-first (vector-trie-ref vt-a i) vt))))

(define (check-vector-trie-length-in-range who vt n)
  (unless (<= n (vector-trie-length vt))
    (raise-arguments-error who "length is out of range"
                           "length" n
                           "valid range" (unquoted-printing-string
                                          (format "[0, ~a]" (vector-trie-length vt)))
                           "vector trie" vt)))

;; TODO: Could be made significantly more efficient by making this a
;; primitive operation that does the whole drop in one go rather than
;; repeatedly dropping single elements.
(define (vector-trie-drop-left vt n)
  (check-vector-trie-length-in-range 'vector-trie-drop-left vt n)
  (for/fold ([vt vt])
            ([i (-in-vector-trie n)])
    (vector-trie-drop-first vt)))

;; TODO: Could be made significantly more efficient by making this a
;; primitive operation that does the whole drop in one go rather than
;; repeatedly dropping single elements.
(define (vector-trie-drop-right vt n)
  (check-vector-trie-length-in-range 'vector-trie-drop-right vt n)
  (for/fold ([vt vt])
            ([i (-in-vector-trie n)])
    (vector-trie-drop-last vt)))
