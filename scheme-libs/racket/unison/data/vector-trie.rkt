#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/fixnum
         racket/match
         racket/sequence
         racket/splicing
         racket/unsafe/ops)

(provide ->fx/wraparound ; used by chunked-seq.rkt

         vector-trie?
         empty-vector-trie
         (contract-out
          [vector-trie-length (-> vector-trie? exact-nonnegative-integer?)]
          [vector-trie-empty? (-> vector-trie? boolean?)]

          [make-vector-trie (-> exact-nonnegative-integer? any/c vector-trie?)]
          [build-vector-trie (-> exact-nonnegative-integer?
                                 procedure? ; should be (-> exact-nonnegative-integer? any/c), but that’s expensive
                                 vector-trie?)]
          [vector->vector-trie (-> vector? vector-trie?)]
          [vector-trie->vector (-> vector-trie? vector?)]

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

         (rename-out [-in-vector-trie in-vector-trie]
                     [-in-reversed-vector-trie in-reversed-vector-trie]))

;; -----------------------------------------------------------------------------

(define (->fx/wraparound v)
  (if (fixnum? v)
      v
      (bitwise-and v (most-positive-fixnum))))

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

    (define ((hash-proc init) vt recur)
      (for/fold ([hc init])
                ([val (-in-vector-trie vt)])
        (fxxor (fx*/wraparound hc 31) (->fx/wraparound (recur val)))))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(define empty-vector-trie (vector-trie 0 0 #f #f))

;; -----------------------------------------------------------------------------
;; bit math operations

(define NODE-BITS 5) ; 32-way branching
(define NODE-CAPACITY (expt 2 NODE-BITS))
(define NODE-INDEX-MASK (sub1 NODE-CAPACITY))

(define (increment-shift shift) (+ shift NODE-BITS))
(define (decrement-shift shift) (- shift NODE-BITS))

;; Returns the capacity of each child of a node at the given level.
(define (node-stride shift)
  (arithmetic-shift 1 shift))

(define (trie-capacity shift)
  (arithmetic-shift 1 (increment-shift shift)))

(define (extract-node-index i shift)
  (bitwise-and (arithmetic-shift i (- shift)) NODE-INDEX-MASK))

(define (restrict-index-to-child i shift)
  (bitwise-and i (sub1 (arithmetic-shift 1 shift))))

(define (restrict-index-to-node i shift)
  (restrict-index-to-child i (increment-shift shift)))

;; Returns an index like `i`, but aligned to point at the first element of the
;; child node at `node-i` (at the level given by `shift`).
(define (set-node-index i shift node-i)
  (define mask (sub1 (arithmetic-shift 1 (increment-shift shift))))
  (bitwise-ior (bitwise-and i (bitwise-not mask))
               (arithmetic-shift node-i shift)))

;; Computes the height of the trie needed to contain `len` elements.
;; `len` must be a positive integer.
(define (trie-length->shift len)
  (define len-log2 (sub1 (integer-length (sub1 len))))
  (- len-log2 (remainder len-log2 NODE-BITS)))

;; -----------------------------------------------------------------------------
;; node operations

(define (make-mutable-node)
  (make-vector NODE-CAPACITY #f))

(define (unsafe-freeze-node! node)
  (unsafe-vector*->immutable-vector! node))

(define (make-node initialize-proc)
  (define node (make-mutable-node))
  (initialize-proc node)
  (unsafe-freeze-node! node))

(define (make-node* initialize-proc)
  (define node (make-mutable-node))
  (define extra-result (initialize-proc node))
  (values (unsafe-freeze-node! node) extra-result))

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

;; Performs a functional update of a vector trie node at the given index.
(define (vector-trie-node-set node shift i val)
  (let loop ([shift shift]
             [node node])
    (define node-i (extract-node-index i shift))
    (vector-trie-node-set/immediate
     node node-i
     (if (zero? shift)
         val
         (loop (decrement-shift shift) (vector-ref node node-i))))))

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
            (loop (decrement-shift shift) child-node)
            (make-singleton-nodes (decrement-shift shift) val))]))))

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
         (loop (decrement-shift shift) (vector-ref node node-i))))))

;; -----------------------------------------------------------------------------
;; core operations

(define (vector-trie-empty? vt)
  (zero? (vector-trie-length vt)))

(define (build-vector-trie len elem-proc)
  (cond
    [(zero? len)
     empty-vector-trie]
    [else
     (define root-shift (trie-length->shift len))
     (vector-trie
      len 0 root-shift
      (let build-trie ([shift root-shift]
                       [i 0])
        (make-node
         (λ (new-node)
           (define step (arithmetic-shift 1 shift))
           (for ([node-i (in-range NODE-CAPACITY)]
                 [i (in-range i len step)])
             (vector-set!
              new-node
              node-i
              (if (zero? shift)
                  (elem-proc i)
                  (build-trie (decrement-shift shift) i))))))))]))

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
          (loop (decrement-shift shift)
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
     (define new-shift (increment-shift shift))
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
     (if (= next-i (arithmetic-shift 1 (increment-shift shift)))

         ;; Out of room, need to allocate a new root.
         (struct-copy
          vector-trie vt
          [length (add1 (vector-trie-length vt))]
          [shift (increment-shift shift)]
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
          [shift (decrement-shift shift)]
          [root-node (vector-ref (vector-trie-root-node vt) (extract-node-index (add1 first-i) shift))])

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
          [shift (decrement-shift shift)]
          [root-node (vector-ref (vector-trie-root-node vt) (extract-node-index (sub1 last-i) shift))])

         ;; Can’t pop the root, just delete an element.
         (struct-copy
          vector-trie vt
          [length new-length]
          [root-node (vector-trie-node-delete (vector-trie-root-node vt) shift last-i #:from-start? #f)]))]))

;; -----------------------------------------------------------------------------
;; appending

;; Returns the index of the first filled element of the given vector
;; trie. The index is relative to the containing leaf, so it is always
;; in the range [0, NODE-CAPACITY).
(define (first-leaf-start-index vt)
  (bitwise-and (vector-trie-offset vt) NODE-INDEX-MASK))

;; Returns the index immediately after the last filled element of the
;; given vector trie. The index is relative to the containing leaf, so
;; it is always in the range (0, NODE-CAPACITY].
(define (last-leaf-end-index vt)
  (add1 (bitwise-and (sub1 (+ (vector-trie-offset vt) (vector-trie-length vt))) NODE-INDEX-MASK)))

(splicing-local [(struct frame (node shift next) #:transparent #:authentic)]
  ;; Builds a procedure that encapsulates an imperative iterator over
  ;; the leaf nodes of the given vector trie, which must not be empty.
  ;; The returned procedure accepts no arguments and returns two values:
  ;; the next leaf in the trie and a boolean indicating whether there
  ;; are any more leaves. If the second result is #f, the procedure
  ;; should not be applied again.
  ;;
  ;; Note that the first and last leaves (which may be the same if there
  ;; is only one leaf) may be only partially filled; the unfilled
  ;; portions may be obtained using `first-leaf-start-index` and
  ;; `last-leaf-end-index`.
  ;;
  ;; If the `reverse?` is not #f, the leaves are iterated in reverse
  ;; order, but otherwise the behavior is the same.
  (define (stream-leaves vt #:reverse? [reverse? #f])
    (cond
      [(zero? (vector-trie-shift vt))
       (define leaf (vector-trie-root-node vt))
       (λ () (values leaf #f))]
      [else
       (define (align-to-leaf n)
         (bitwise-and n (bitwise-not NODE-INDEX-MASK)))
       (define first-i (align-to-leaf (vector-trie-offset vt)))
       (define last-i (align-to-leaf (sub1 (+ (vector-trie-offset vt) (vector-trie-length vt)))))

       (define-values [node-bound next-i]
         (if reverse?
             (values 0 last-i)
             (values (sub1 NODE-CAPACITY) first-i)))

       (define stack #f)
       (define node (vector-trie-root-node vt))

       ;; Descends into the trie until `node` is a node at the level just
       ;; above the leaves, pushing frames onto `stack` as appropriate.
       (define (descend! shift)
         (unless (= shift NODE-BITS)
           (define node-i (extract-node-index next-i shift))
           ;; If we’re descending into the last element of this node, we don’t
           ;; want to return to this node ever again, so omit the stack frame
           ;; (effectively making a tail call).
           (unless (= node-i node-bound)
             (set! stack (frame node shift stack)))
           (set! node (vector-ref node node-i))
           (descend! (decrement-shift shift))))

       ;; Ascends to the parent node, then descends into the next child.
       (define (focus-next-node!)
         (define shift (frame-shift stack))
         (define node-i (extract-node-index next-i shift))
         (set! node (vector-ref (frame-node stack) node-i))

         ;; Pop the stack frame if we’re never going to return here again,
         ;; otherwise leave it in place to avoid a redundant allocation.
         (when (= node-i node-bound)
           (set! stack (frame-next stack)))

         (descend! (decrement-shift shift)))

       (define (get-next-leaf!)
         (define node-i (extract-node-index next-i NODE-BITS))
         (define leaf (vector-ref node node-i))
         (set! next-i (if reverse?
                          (- next-i NODE-CAPACITY)
                          (+ next-i NODE-CAPACITY)))
         (cond
           [(if reverse? (< next-i first-i) (> next-i last-i))
            (values leaf #f)]
           [else
            (when (= node-i node-bound)
              (focus-next-node!))
            (values leaf #t)]))

       (descend! (vector-trie-shift vt))
       get-next-leaf!])))

(define (vector-trie-append vt-a vt-b)
  (define a-len (vector-trie-length vt-a))
  (define b-len (vector-trie-length vt-b))
  (cond
    [(zero? a-len) vt-b]
    [(zero? b-len) vt-a]
    [else
     (define new-len (+ a-len b-len))
     (cond
       [(>= a-len b-len)
        (define a-offset (vector-trie-offset vt-a))
        (define a-shift (vector-trie-shift vt-a))

        (define first-leaf-start (first-leaf-start-index vt-b))
        (define last-leaf-end (last-leaf-end-index vt-b))

        (define a-last-leaf-slop (- NODE-CAPACITY (last-leaf-end-index vt-a)))
        (define leaf-split-i (bitwise-and (+ first-leaf-start a-last-leaf-slop) NODE-INDEX-MASK))
        (define leaf-insert-i (- NODE-CAPACITY leaf-split-i))

        (define get-next-leaf! (stream-leaves vt-b))
        (define-values [leaf more?] (get-next-leaf!))
        (define (next-leaf!)
          (set!-values [leaf more?] (get-next-leaf!)))

        (define new-end-i (+ a-offset new-len))
        (define new-shift (trie-length->shift new-end-i))

        ;; Builds a subtrie of a given height that *exclusively* contains
        ;; values from `vt-b`. If there aren’t enough values left in
        ;; `vt-b` to fill the subtrie, then the result will be only
        ;; partially full.
        (define (build-new-subtrie shift)
          (match-define-values [child _]
            (let loop ([shift shift])
              (cond
                [(zero? shift)
                 (cond
                   ;; As an optimization, if `leaf-split-i` is 0, then we can reuse
                   ;; leaf nodes from `vt-b` without slicing and copying.
                   [(zero? leaf-split-i)
                    (define this-leaf leaf)
                    (if more?
                        (begin0
                          (values leaf #t)
                          (next-leaf!))
                        (values leaf #f))]

                   ;; Otherwise, we need to combine elements from the current
                   ;; leaf and the next one.
                   [else
                    (make-node*
                     (λ (new-leaf)
                       (vector-copy! new-leaf 0 leaf leaf-split-i)
                       (cond
                         [more?
                          (next-leaf!)
                          (vector-copy! new-leaf leaf-insert-i leaf 0 leaf-split-i)
                          (or more? (< leaf-split-i last-leaf-end))]
                         [else
                          #f])))])]
                [else
                 (make-node*
                  (λ (new-node)
                    (define child-shift (decrement-shift shift))
                    (for/and ([i (in-range NODE-CAPACITY)])
                      (define-values [child more?] (loop child-shift))
                      (vector-set! new-node i child)
                      more?)))])))
          child)

        ;; Inserts subtries containing elements from `vt-b` into the given
        ;; node, starting at the given index. The node must already contain an
        ;; element at the previous index.
        (define (insert-subtries node shift i)
          (cond
            [(zero? shift)
             (define leaf-i (extract-node-index i 0))
             (make-node
              (λ (new-leaf)
                (vector-copy! new-leaf 0 node 0 leaf-i)
                (cond
                  [(< leaf-i first-leaf-start)
                   (vector-copy! new-leaf leaf-i leaf first-leaf-start)
                   (when more?
                     (next-leaf!)
                     (vector-copy! new-leaf leaf-split-i leaf 0 leaf-split-i))]
                  [else
                   (vector-copy! new-leaf leaf-i leaf first-leaf-start leaf-split-i)])))]
            [else
             (make-node
              (λ (new-node)
                (define node-i (extract-node-index i shift))
                (vector-copy! new-node 0 node 0 node-i)

                (define child-shift (decrement-shift shift))
                (define-values [i* node-i*]
                  (cond
                    [(zero? (restrict-index-to-child i shift))
                     (values i node-i)]
                    [else
                     (vector-set! new-node node-i (insert-subtries (vector-ref node node-i) child-shift i))
                     (values (set-node-index i shift (add1 node-i)) (add1 node-i))]))

                (for ([i (in-range i* new-end-i (node-stride shift))]
                      [node-i (in-range node-i* NODE-CAPACITY)])
                  (vector-set! new-node node-i (build-new-subtrie child-shift)))))]))

        (vector-trie
         new-len
         a-offset
         new-shift
         (let root-loop ([shift new-shift]
                         [i (+ a-offset a-len)])
           (cond
             [(= shift a-shift)
              (if (zero? (restrict-index-to-node i shift))
                  (vector-trie-root-node vt-a)
                  (insert-subtries (vector-trie-root-node vt-a) shift i))]
             [else
              (make-node
               (λ (new-node)
                 (define child-shift (decrement-shift shift))
                 (vector-set! new-node 0 (root-loop child-shift i))
                 (for ([i (in-range (set-node-index i shift 1) new-end-i (node-stride shift))]
                       [node-i (in-range 1 NODE-CAPACITY)])
                   (vector-set! new-node node-i (build-new-subtrie child-shift)))))])))]

       [else
        (define b-offset (vector-trie-offset vt-b))
        (define b-shift (vector-trie-shift vt-b))

        (define first-leaf-start (first-leaf-start-index vt-a))
        (define last-leaf-end (last-leaf-end-index vt-a))

        (define leaf-split-i (bitwise-and (- last-leaf-end (first-leaf-start-index vt-b)) NODE-INDEX-MASK))
        (define leaf-insert-i (- NODE-CAPACITY leaf-split-i))

        (define get-prev-leaf! (stream-leaves vt-a #:reverse? #t))
        (define-values [leaf more?] (get-prev-leaf!))
        (define (prev-leaf!)
          (set!-values [leaf more?] (get-prev-leaf!)))

        (define b-slop (- (trie-capacity b-shift) (+ b-offset b-len)))
        (define new-shift (trie-length->shift (+ b-slop new-len)))
        (define new-offset (- (trie-capacity new-shift) b-slop new-len))

        ;; Builds a subtrie of a given height that *exclusively* contains
        ;; values from `vt-a`. If there aren’t enough values left in
        ;; `vt-a` to fill the subtrie, then the result will be only
        ;; partially full.
        (define (build-new-subtrie shift)
          (match-define-values [child _]
            (let loop ([shift shift])
              (cond
                [(zero? shift)
                 (cond
                   ;; As an optimization, if `leaf-split-i` is 0, then we can reuse
                   ;; leaf nodes from `vt-a` without slicing and copying.
                   [(zero? leaf-split-i)
                    (define this-leaf leaf)
                    (if more?
                        (begin0
                          (values leaf #t)
                          (prev-leaf!))
                        (values leaf #f))]

                   ;; Otherwise, we need to combine elements from the current
                   ;; leaf and the next one.
                   [else
                    (make-node*
                     (λ (new-leaf)
                       (vector-copy! new-leaf leaf-insert-i leaf 0 leaf-split-i)
                       (cond
                         [more?
                          (prev-leaf!)
                          (vector-copy! new-leaf 0 leaf leaf-split-i)
                          (or more? (>= leaf-split-i first-leaf-start))]
                         [else
                          #f])))])]
                [else
                 (make-node*
                  (λ (new-node)
                    (define child-shift (decrement-shift shift))
                    (for/and ([i (in-inclusive-range (sub1 NODE-CAPACITY) 0 -1)])
                      (define-values [child more?] (loop child-shift))
                      (vector-set! new-node i child)
                      more?)))])))
          child)

        ;; Inserts subtries containing elements from `vt-a` into the given
        ;; node, starting at the given index. The node must already contain an
        ;; element at the following index.
        (define (insert-subtries node shift i)
          (cond
            [(zero? shift)
             (define leaf-i (extract-node-index i 0))
             (make-node
              (λ (new-leaf)
                (vector-copy! new-leaf (add1 leaf-i) node (add1 leaf-i))
                (cond
                  [(>= leaf-i last-leaf-end)
                   (vector-copy! new-leaf (- (add1 leaf-i) last-leaf-end) leaf 0 last-leaf-end)
                   (when more?
                     (prev-leaf!)
                     (vector-copy! new-leaf 0 leaf leaf-split-i))]
                  [else
                   (vector-copy! new-leaf 0 leaf leaf-split-i last-leaf-end)])))]
            [else
             (make-node
              (λ (new-node)
                (define node-i (extract-node-index i shift))
                (vector-copy! new-node (add1 node-i) node (add1 node-i))

                (define child-shift (decrement-shift shift))
                (define-values [i* node-i*]
                  (cond
                    [(zero? (restrict-index-to-child (add1 i) shift))
                     (values i node-i)]
                    [else
                     (vector-set! new-node node-i (insert-subtries (vector-ref node node-i) child-shift i))
                     (values (sub1 (set-node-index i shift node-i)) (sub1 node-i))]))

                (for ([i (in-inclusive-range i* new-offset (- (node-stride shift)))]
                      [node-i (in-inclusive-range node-i* 0 -1)])
                  (vector-set! new-node node-i (build-new-subtrie child-shift)))))]))

        (vector-trie
         new-len
         new-offset
         new-shift
         (let root-loop ([shift new-shift]
                         [i (sub1 (+ new-offset a-len))])
           (cond
             [(= shift b-shift)
              (if (zero? (restrict-index-to-node (add1 i) shift))
                  (vector-trie-root-node vt-b)
                  (insert-subtries (vector-trie-root-node vt-b) shift i))]
             [else
              (make-node
               (λ (new-node)
                 (define child-shift (decrement-shift shift))
                 (vector-set! new-node (sub1 NODE-CAPACITY) (root-loop child-shift i))
                 (for ([i (in-inclusive-range (sub1 (set-node-index i shift (sub1 NODE-CAPACITY))) new-offset (- (node-stride shift)))]
                       [node-i (in-inclusive-range (- NODE-CAPACITY 2) 0 -1)])
                   (vector-set! new-node node-i (build-new-subtrie child-shift)))))])))])]))

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

;; TODO: Could be made more efficient by directly walking the internal
;; structure, avoiding repeated traversals.
(define (in-reversed-vector-trie vt)
  (unless (vector-trie? vt)
    (raise-argument-error 'in-reversed-vector-trie "vector-trie?" vt))
  (sequence-map (λ (i) (vector-trie-ref vt i))
                (in-inclusive-range (sub1 (vector-trie-length vt)) 0 -1)))
(define-sequence-syntax -in-reversed-vector-trie
  (λ () #'in-reversed-vector-trie)
  (syntax-parser
    [[(x:id) (_ {~var vt-e (expr/c #'vector-trie?)})]
     #'[(x) (:do-in
             ([(vt vt-len) (let ([vt vt-e.c])
                             (values vt (vector-trie-length vt)))])
             (void)
             ([i (sub1 (vector-trie-length vt))])
             (>= i 0)
             ([(x) (vector-trie-ref vt i)])
             #t
             #t
             [(sub1 i)])]]
    [_ #f]))

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
            ([i (in-range n)])
    (vector-trie-drop-first vt)))

;; TODO: Could be made significantly more efficient by making this a
;; primitive operation that does the whole drop in one go rather than
;; repeatedly dropping single elements.
(define (vector-trie-drop-right vt n)
  (check-vector-trie-length-in-range 'vector-trie-drop-right vt n)
  (for/fold ([vt vt])
            ([i (in-range n)])
    (vector-trie-drop-last vt)))

(define (make-vector-trie len elem)
  (build-vector-trie len (λ (i) elem)))

;; TODO: Could be made more efficient by creating nodes via block copies.
(define (vector->vector-trie vec)
  (build-vector-trie
   (vector-length vec)
   (λ (i) (vector-ref vec i))))

;; TODO: Could be made more efficient by doing block copies.
(define (vector-trie->vector vt)
  (define vec (make-vector (vector-trie-length vt) #f))
  (for ([(val i) (in-indexed (-in-vector-trie vt))])
    (vector-set! vec i val))
  (unsafe-vector*->immutable-vector! vec))
