#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/contract
         racket/fixnum
         racket/match
         racket/sequence
         racket/unsafe/ops
         syntax/parse/define
         "vector-trie.rkt")

;; Generates specialized code for a chunked sequence type. Only intended to be
;; used to generate the `chunked-list`, `chunked-string`, and `chunked-bytes`
;; types.
;;
;; The internal structure of each chunked sequence is built upon a vector trie
;; (from "vector-trie.rkt"). That structure is extended in the following ways:
;;
;;   * Most obviously, each variant of chunked sequence uses its own type of
;;     chunk: chunked lists use vectors, chunked strings use strings, and
;;     chunked bytes use bytestrings.
;;
;;   * Chunked sequences that fit in a single chunk always use a specialized
;;     `single-chunk` internal representation (see Note [chunks-length invariant]).
;;
;;   * Larger chunked sequences store the first and last chunk directly in the
;;     root node, so only the middle chunks are stored in the actual vector
;;     trie. This accelerates operations that only need to look at the first or
;;     last chunk.
;;
;; Each use of `define-chunked-sequence` generates and exports the following
;; API, replacing each occurrence of `chunked-seq` with the name of the
;; specialized sequence, each occurrence of `chunk` with the name of the
;; underlying chunk type, and each occurrence of `elem/c` with the contract that
;; elements of the specialized sequence must conform to:
;;
;;   chunked-seq? : (-> any/c boolean?)
;;   empty-chunked-seq : chunked-seq?
;;   chunked-seq-length : (-> chunked-seq? exact-nonnegative-integer?)
;;   chunked-seq-empty? : (-> chunked-seq? boolean?)
;;
;;   make-chunked-seq : (-> exact-nonnegative-integer? elem/c chunked-seq?)
;;   build-chunked-seq : (-> exact-nonnegative-integer? (-> exact-nonnegative-integer? elem/c) chunked-seq?)
;;   chunk->chunked-seq : (-> chunk? chunked-seq?)
;;   chunked-seq->chunk : (-> chunked-seq? chunk?)
;;
;;   chunked-seq-ref : (-> chunked-seq? exact-nonnegative-integer? elem/c)
;;   chunked-seq-set : (-> chunked-seq? exact-nonnegative-integer? elem/c chunked-seq?)
;;   chunked-seq-add-first, chunked-seq-add-last : (-> chunked-seq? elem/c chunked-seq?)
;;   chunked-seq-drop-first, chunked-seq-drop-last :
;;     (-> (and/c chunked-seq? (not/c chunked-seq-empty?)) chunked-seq?)
;;
;;   chunked-seq-append : (-> chunked-seq? ... chunked-seq?)
;;
;;   chunked-seq=?/recur : (-> chunked-seq? chunked-seq? (-> any/c any/c any/c) boolean?)
;;   chunked-seq-compare/recur : (-> chunked-seq? chunked-seq? (-> any/c any/c ordering/c) ordering/c)
;;
;;   in-chunked-seq : (-> chunked-seq? (sequence/c elem/c))
;;   in-chunked-seq-chunks : (-> chunked-seq? (sequence/c chunk?))
;;     Note: Like `in-list`, `in-chunked-seq` and `in-chunked-seq-chunks` can
;;     provide better performance when they appear directly in a `for` clause.
;;
(define-syntax-parser define-chunked-sequence
  [(_ chunked-seq:id
      #:element-contract elem/c:id
      #:chunk-type chunk-type:id
      #:chunk-bits CHUNK-BITS:nat
      #:chunk-immutable! unsafe-chunk->immutable-chunk!:id)

   (define (derived-seq-id pattern)
     (format-id #'chunked-seq pattern #'chunked-seq))
   (define (derived-chunk-id pattern)
     (format-id #'chunk-type pattern #'chunk-type #:source #'chunk-type #:props #'chunk-type))

   (define/with-syntax CHUNK-CAPACITY (expt 2 (syntax-e #'CHUNK-BITS)))
   (define/with-syntax CHUNK-INDEX-MASK (sub1 (syntax-e #'CHUNK-CAPACITY)))

   (define/with-syntax chunk? (derived-chunk-id "~a?"))
   (define/with-syntax chunk-length (derived-chunk-id "~a-length"))
   (define/with-syntax make-mutable-chunk (derived-chunk-id "make-~a"))
   (define/with-syntax chunk-ref (derived-chunk-id "~a-ref"))
   (define/with-syntax chunk-set! (derived-chunk-id "~a-set!"))
   (define/with-syntax chunk-copy! (derived-chunk-id "~a-copy!"))
   (define/with-syntax in-chunk (derived-chunk-id "in-~a"))

   (define/with-syntax chunked-seq? (derived-seq-id "~a?"))
   (define/with-syntax empty-chunked-seq (derived-seq-id "empty-~a"))
   (define/with-syntax chunked-seq-empty? (derived-seq-id "~a-empty?"))
   (define/with-syntax chunked-seq-length (derived-seq-id "~a-length"))

   (define/with-syntax make-chunked-seq (derived-seq-id "make-~a"))
   (define/with-syntax build-chunked-seq (derived-seq-id "build-~a"))
   (define/with-syntax chunk->chunked-seq (format-id #'chunked-seq "~a->~a" #'chunk-type #'chunked-seq))
   (define/with-syntax chunked-seq->chunk (format-id #'chunked-seq "~a->~a" #'chunked-seq #'chunk-type))

   (define/with-syntax chunked-seq-ref (derived-seq-id "~a-ref"))
   (define/with-syntax chunked-seq-set (derived-seq-id "~a-set"))
   (define/with-syntax chunked-seq-add-first (derived-seq-id "~a-add-first"))
   (define/with-syntax chunked-seq-add-last (derived-seq-id "~a-add-last"))
   (define/with-syntax chunked-seq-drop-first (derived-seq-id "~a-drop-first"))
   (define/with-syntax chunked-seq-drop-last (derived-seq-id "~a-drop-last"))

   (define/with-syntax chunked-seq-append (derived-seq-id "~a-append"))

   (define/with-syntax chunked-seq=?/recur (derived-seq-id "~a=?/recur"))
   (define/with-syntax chunked-seq-compare/recur (derived-seq-id "~a-compare/recur"))

   (define/with-syntax in-chunked-seq (derived-seq-id "in-~a"))
   (define/with-syntax in-chunked-seq-chunks (derived-seq-id "in-~a-chunks"))

   #`(begin
       (provide chunked-seq?
                empty-chunked-seq
                (contract-out
                 [chunked-seq-length (-> chunked-seq? exact-nonnegative-integer?)]
                 [chunked-seq-empty? (-> chunked-seq? boolean?)]

                 [make-chunked-seq (-> exact-nonnegative-integer? elem/c chunked-seq?)]
                 [build-chunked-seq (-> exact-nonnegative-integer?
                                        procedure? ; should be (-> exact-nonnegative-integer? elem/c), but that’s expensive
                                        chunked-seq?)]
                 [chunk->chunked-seq (-> chunk? chunked-seq?)]
                 [chunked-seq->chunk (-> chunked-seq? chunk?)]

                 [chunked-seq-ref (-> chunked-seq? exact-nonnegative-integer? elem/c)]
                 [chunked-seq-set (-> chunked-seq? exact-nonnegative-integer? elem/c chunked-seq?)]
                 [chunked-seq-add-first (-> chunked-seq? elem/c chunked-seq?)]
                 [chunked-seq-add-last (-> chunked-seq? elem/c chunked-seq?)]
                 [chunked-seq-drop-first (-> (and/c chunked-seq? (not/c chunked-seq-empty?)) chunked-seq?)]
                 [chunked-seq-drop-last (-> (and/c chunked-seq? (not/c chunked-seq-empty?)) chunked-seq?)]

                 [chunked-seq-append {... (-> chunked-seq? ... chunked-seq?)}]

                 [chunked-seq=?/recur (-> chunked-seq? chunked-seq? procedure? boolean?)]
                 [chunked-seq-compare/recur (-> chunked-seq?
                                                chunked-seq?
                                                procedure? ; should be (-> any/c any/c ordering/c), but that’s expensive
                                                (or/c '= '< '>))])

                (rename-out
                 [-in-chunked-seq in-chunked-seq]
                 [-in-chunked-seq-chunks in-chunked-seq-chunks]))

       ;; ----------------------------------------------------------------------
       ;; chunk operations

       (define (make-chunk len init-proc)
         (define chunk (make-mutable-chunk len))
         (init-proc chunk)
         (unsafe-chunk->immutable-chunk! chunk))

       (define (build-chunk len elem-proc)
         (make-chunk
          len
          (λ (chunk)
            (for ([i (in-range len)])
              (chunk-set! chunk i (elem-proc i))))))

       (define (chunk-set chunk i val)
         (make-chunk
          (chunk-length chunk)
          (λ (new-chunk)
            (chunk-copy! new-chunk 0 chunk 0 i)
            (chunk-set! new-chunk i val)
            (chunk-copy! new-chunk (add1 i) chunk (add1 i)))))

       (define (chunk-add-first chunk val)
         (make-chunk
          (add1 (chunk-length chunk))
          (λ (new-chunk)
            (chunk-set! new-chunk 0 val)
            (chunk-copy! new-chunk 1 chunk 0))))

       (define (chunk-add-last chunk val)
         (define old-len (chunk-length chunk))
         (make-chunk
          (add1 old-len)
          (λ (new-chunk)
            (chunk-copy! new-chunk 0 chunk 0)
            (chunk-set! new-chunk old-len val))))

       (define (chunk-drop-first chunk)
         (make-chunk
          (sub1 (chunk-length chunk))
          (λ (new-chunk)
            (chunk-copy! new-chunk 0 chunk 1))))

       (define (chunk-drop-last chunk)
         (define new-len (sub1 (chunk-length chunk)))
         (make-chunk
          new-len
          (λ (new-chunk)
            (chunk-copy! new-chunk 0 chunk 0 new-len))))

       (define (singleton-chunk val)
         (make-chunk 1 (λ (chunk) (chunk-set! chunk 0 val))))

       (define (pair-chunk a b)
         (make-chunk 2 (λ (chunk)
                         (chunk-set! chunk 0 a)
                         (chunk-set! chunk 1 b))))

       (define (chunk-slice chunk start [end (chunk-length chunk)])
         (make-chunk
          (- end start)
          (λ (new-chunk)
            (chunk-copy! new-chunk 0 chunk start end))))

       (define (chunk-append a b)
         (define len-a (chunk-length a))
         (make-chunk
          (+ len-a (chunk-length b))
          (λ (chunk)
            (chunk-copy! chunk 0 a 0)
            (chunk-copy! chunk len-a b 0))))

       ;; Given two chunks that together have more than CHUNK-CAPACITY
       ;; elements, moves elements from the end of the first chunk into
       ;; the start of the second chunk until it is full.
       (define (chunk-fill-right a b)
         (define elems-to-move (- CHUNK-CAPACITY (chunk-length b)))
         (cond
           [(zero? elems-to-move)
            (values a b)]
           [else
            (define new-len-a (- (chunk-length a) elems-to-move))
            (values
             (chunk-slice a 0 new-len-a)
             (make-chunk
              CHUNK-CAPACITY
              (λ (new-b)
                (chunk-copy! new-b 0 a new-len-a)
                (chunk-copy! new-b elems-to-move b 0))))]))

       ;; Like `chunk-fill-right`, but moves elements from the start
       ;; of the second chunk into the end of the first chunk.
       (define (chunk-fill-left a b)
         (define len-a (chunk-length a))
         (define elems-to-move (- CHUNK-CAPACITY len-a))
         (cond
           [(zero? elems-to-move)
            (values a b)]
           [else
            (values
             (make-chunk
              CHUNK-CAPACITY
              (λ (new-a)
                (chunk-copy! new-a 0 a 0)
                (chunk-copy! new-a len-a b 0 elems-to-move)))
             (chunk-slice b elems-to-move))]))

       (define (chunk-trie-elem-length vt)
         (arithmetic-shift (vector-trie-length vt) CHUNK-BITS))

       (define (chunk-trie-elem-ref vt i)
         (chunk-ref (vector-trie-ref vt (arithmetic-shift i (- CHUNK-BITS)))
                    (bitwise-and i CHUNK-INDEX-MASK)))

       (define (chunk-trie-elem-set vt i val)
         (vector-trie-update
          vt
          (arithmetic-shift i (- CHUNK-BITS))
          (λ (chunk) (chunk-set chunk (bitwise-and i CHUNK-INDEX-MASK) val))))

       ;; ----------------------------------------------------------------------
       ;; core operations

       ;; Supertype structure, never instantiated directly.
       (struct chunked-seq ()
         #:transparent
         #:reflection-name 'chunked-seq
         #:property prop:equal+hash
         (let ()
           (define (equal-proc cs-a cs-b recur)
             (chunked-seq=?/recur cs-a cs-b recur))

           (define ((hash-proc init) cs recur)
             (for/fold ([hc init])
                       ([val (-in-chunked-seq cs)])
               (fxxor (fx*/wraparound hc 31) (->fx/wraparound (recur val)))))

           (list equal-proc (hash-proc 3) (hash-proc 5))))

       (define empty-chunked-seq
         (let ()
           (struct empty-chunked-seq chunked-seq ())
           (empty-chunked-seq)))

       (define (chunked-seq-empty? v)
         (eq? v empty-chunked-seq))

       (struct single-chunk chunked-seq (chunk)
         #:transparent
         #:reflection-name 'chunked-seq)

       ;; Note [chunks-length invariant]
       ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       ;; We maintain the invariant that a `chunks` structure always
       ;; has more than CHUNK-CAPACITY elements. This means we MUST
       ;; collapse `first-chunk` and `last-chunk` into a `single-chunk`
       ;; if we don’t have enough elements.

       (struct chunks chunked-seq
         (length ; see Note [chunks-length invariant]
          first-chunk
          chunk-trie
          last-chunk)
         #:transparent
         #:reflection-name 'chunked-seq)

       (define (chunked-seq-length cs)
         (match cs
           [(? chunked-seq-empty?) 0]
           [(single-chunk chunk) (chunk-length chunk)]
           [_ (chunks-length cs)]))

       (define (make-chunked-seq len elem)
         (build-chunked-seq len (λ (i) elem)))

       (define (build-chunked-seq len elem-proc)
         (cond
           [(zero? len)
            empty-chunked-seq]
           [(<= len CHUNK-CAPACITY)
            (single-chunk (build-chunk len elem-proc))]
           [else
            (define-values [full-chunk-count leftover-count] (quotient/remainder len CHUNK-CAPACITY))
            (define last-offset (* (if (zero? leftover-count) (sub1 full-chunk-count) full-chunk-count) CHUNK-CAPACITY))
            (chunks len
                    (build-chunk CHUNK-CAPACITY elem-proc)
                    (build-vector-trie
                     (- full-chunk-count (if (zero? leftover-count) 2 1))
                     (λ (chunk-i)
                       (define offset (* (add1 chunk-i) CHUNK-CAPACITY))
                       (build-chunk CHUNK-CAPACITY (λ (i) (elem-proc (+ offset i))))))
                    (build-chunk
                     (if (zero? leftover-count) CHUNK-CAPACITY leftover-count)
                     (λ (i) (elem-proc (+ last-offset i)))))]))

       (define (check-index-in-range who cs i)
         (define len (chunked-seq-length cs))
         (unless (< i len)
           (raise-range-error who 'chunked-seq? "" i cs 0 (sub1 len))))

       (define (chunked-seq-ref cs i)
         (check-index-in-range 'chunked-seq-ref cs i)
         (match cs
           [(single-chunk chunk)
            (chunk-ref chunk i)]
           [(chunks _ first-c vt last-c)
            (define first-len (chunk-length first-c))
            (cond
              [(< i first-len)
               (chunk-ref first-c i)]
              [else
               (define trie-i (- i first-len))
               (define vt-len (chunk-trie-elem-length vt))
               (if (< trie-i vt-len)
                   (chunk-trie-elem-ref vt trie-i)
                   (chunk-ref last-c (- trie-i vt-len)))])]))

       (define (chunked-seq-set cs i val)
         (check-index-in-range 'chunked-seq-set cs i)
         (match cs
           [(single-chunk chunk)
            (single-chunk (chunk-set chunk i val))]
           [(chunks _ first-c vt last-c)
            (define first-len (chunk-length first-c))
            (cond
              [(< i first-len)
               (struct-copy
                chunks cs
                [first-chunk (chunk-set first-c i val)])]
              [else
               (define trie-i (- i first-len))
               (define vt-len (chunk-trie-elem-length vt))
               (if (< trie-i vt-len)
                   (struct-copy
                    chunks cs
                    [chunk-trie (chunk-trie-elem-set vt trie-i val)])
                   (struct-copy
                    chunks cs
                    [last-chunk (chunk-set last-c (- trie-i vt-len) val)]))])]))

       (define (chunked-seq-add-first cs val)
         (match cs
           [(? chunked-seq-empty?)
            (single-chunk (singleton-chunk val))]

           [(single-chunk chunk)
            (define len (chunk-length chunk))
            (if (< len CHUNK-CAPACITY)
                (single-chunk (chunk-add-first chunk val))
                (chunks (add1 len)
                        (singleton-chunk val)
                        empty-vector-trie
                        chunk))]

           [(chunks len first-c vt _)
            (if (< (chunk-length first-c) CHUNK-CAPACITY)
                (struct-copy
                 chunks cs
                 [length (add1 len)]
                 [first-chunk (chunk-add-first first-c val)])
                (struct-copy
                 chunks cs
                 [length (add1 len)]
                 [first-chunk (singleton-chunk val)]
                 [chunk-trie (vector-trie-add-first vt first-c)]))]))

       (define (chunked-seq-add-last cs val)
         (match cs
           [(? chunked-seq-empty?)
            (single-chunk (singleton-chunk val))]

           [(single-chunk chunk)
            (define len (chunk-length chunk))
            (if (< len CHUNK-CAPACITY)
                (single-chunk (chunk-add-last chunk val))
                (chunks (add1 len)
                        chunk
                        empty-vector-trie
                        (singleton-chunk val)))]

           [(chunks len _ vt last-c)
            (if (< (chunk-length last-c) CHUNK-CAPACITY)
                (struct-copy
                 chunks cs
                 [length (add1 len)]
                 [last-chunk (chunk-add-last last-c val)])
                (struct-copy
                 chunks cs
                 [length (add1 len)]
                 [chunk-trie (vector-trie-add-last vt last-c)]
                 [last-chunk (singleton-chunk val)]))]))

       (define (chunked-seq-drop-first cs)
         (match cs
           [(single-chunk chunk)
            (if (= (chunk-length chunk) 1)
                empty-chunked-seq
                (single-chunk (chunk-drop-first chunk)))]

           [(chunks len first-c vt last-c)
            (define new-len (sub1 len))
            (define first-len (chunk-length first-c))
            (cond
              [(<= new-len CHUNK-CAPACITY)
               ;; Not enough elements; must collapse into a single
               ;; chunk (see Note [chunks-length invariant]).
               (single-chunk
                (if (= first-len 1)
                    last-c
                    (make-chunk
                     new-len
                     (λ (chunk)
                       (chunk-copy! chunk 0 first-c 1)
                       (chunk-copy! chunk first-len last-c 0)))))]
              [(= first-len 1)
               (define-values [vt* first-c*] (vector-trie-pop-first vt))
               (struct-copy
                chunks cs
                [length (sub1 len)]
                [first-chunk first-c*]
                [chunk-trie vt*])]
              [else
               (struct-copy
                chunks cs
                [length (sub1 len)]
                [first-chunk (chunk-drop-first last-c)])])]))

       (define (chunked-seq-drop-last cs)
         (match cs
           [(single-chunk chunk)
            (if (= (chunk-length chunk) 1)
                empty-chunked-seq
                (single-chunk (chunk-drop-last chunk)))]

           [(chunks len first-c vt last-c)
            (define new-len (sub1 len))
            (define last-len (chunk-length last-c))
            (cond
              [(<= new-len CHUNK-CAPACITY)
               ;; Not enough elements; must collapse into a single
               ;; chunk (see Note [chunks-length invariant]).
               (single-chunk
                (if (= last-len 1)
                    first-c
                    (make-chunk
                     new-len
                     (λ (chunk)
                       (chunk-copy! chunk 0 first-c 0)
                       (chunk-copy! chunk (chunk-length first-c) last-c 0 (sub1 last-len))))))]
              [(= last-len 1)
               (define-values [vt* last-c*] (vector-trie-pop-last vt))
               (struct-copy
                chunks cs
                [length (sub1 len)]
                [chunk-trie vt*]
                [last-chunk last-c*])]
              [else
               (struct-copy
                chunks cs
                [length (sub1 len)]
                [last-chunk (chunk-drop-last last-c)])])]))

       ;; ----------------------------------------------------------------------
       ;; appending

       (define chunked-seq-append
         (case-lambda
           [() empty-chunked-seq]
           [(cs) cs]

           [(cs-a cs-b)
            (match* {cs-a cs-b}
              [{(? chunked-seq-empty?) _} cs-b]
              [{_ (? chunked-seq-empty?)} cs-a]

              [{(single-chunk chunk-a) (single-chunk chunk-b)}
               (define len (+ (chunk-length chunk-a) (chunk-length chunk-b)))
               ;; see Note [chunks-length invariant]
               (if (< len CHUNK-CAPACITY)
                   (single-chunk (chunk-append chunk-a chunk-b))
                   (chunks len chunk-a empty-vector-trie chunk-b))]

              [{(single-chunk chunk) (chunks len first-c vt _)}
               (cond
                 [(< (+ (chunk-length chunk) (chunk-length first-c)) CHUNK-CAPACITY)
                  (struct-copy
                   chunks cs-b
                   [length (+ (chunk-length chunk) len)]
                   [first-chunk (chunk-append chunk first-c)])]
                 [else
                  (define-values [first-c* full-chunk] (chunk-fill-right chunk first-c))
                  (struct-copy
                   chunks cs-b
                   [length (+ (chunk-length chunk) len)]
                   [first-chunk first-c*]
                   [chunk-trie (vector-trie-add-first vt full-chunk)])])]

              [{(chunks len _ vt last-c) (single-chunk chunk)}
               (cond
                 [(< (+ (chunk-length last-c) (chunk-length chunk)) CHUNK-CAPACITY)
                  (struct-copy
                   chunks cs-a
                   [length (+ len (chunk-length chunk))]
                   [last-chunk (chunk-append last-c chunk)])]
                 [else
                  (define-values [full-chunk last-c*] (chunk-fill-left last-c chunk))
                  (struct-copy
                   chunks cs-a
                   [length (+ len (chunk-length chunk))]
                   [chunk-trie (vector-trie-add-last vt full-chunk)]
                   [last-chunk last-c*])])]

              [{(chunks len-a first-a vt-a last-a) (chunks len-b first-b vt-b last-b)}
               (define new-len (+ len-a len-b))
               (define last-a-len (chunk-length last-a))
               (define first-b-len (chunk-length first-b))
               (define middle-len (+ last-a-len first-b-len))
               (cond
                 ;; If the last chunk of the first sequence and the first chunk of the
                 ;; second sequence are both full chunks, we can use `vector-trie-append`.
                 [(and (= last-a-len CHUNK-CAPACITY)
                       (= first-b-len CHUNK-CAPACITY))
                  (chunks new-len
                          first-a
                          (vector-trie-append
                           (vector-trie-add-last (vector-trie-add-last vt-a last-a) first-b)
                           vt-b)
                          last-b)]

                 ;; If the last chunk of the first sequence and the first chunk of the
                 ;; second sequence can be combined to make a single full chunk, we can
                 ;; use `vector-trie-append`.
                 [(= middle-len CHUNK-CAPACITY)
                  (define middle-chunk (chunk-append last-a first-b))
                  (chunks new-len
                          first-a
                          (vector-trie-append (vector-trie-add-last vt-a middle-chunk) vt-b)
                          last-b)]

                 ;; Otherwise, we have to do a series of splits and copies for each
                 ;; chunk. To minimize overhead, we want to use the longer sequence
                 ;; as the base sequence to prepend or append to.
                 [(< len-a len-b)
                  ;; Prepend case: transfer chunks from `vt-a` into `vt-b`.
                  (define new-vt vt-b)
                  (define new-chunk (make-mutable-chunk CHUNK-CAPACITY))
                  (define (transfer-chunk! #:done? [done? #f])
                    (set! new-vt (vector-trie-add-first new-vt (unsafe-chunk->immutable-chunk! new-chunk)))
                    (set! new-chunk (if done? #f (make-mutable-chunk CHUNK-CAPACITY))))

                  ;; Transfer `first-b` and `last-a` and compute where to split
                  ;; each full chunk in `vt-a`.
                  (define split-i
                    (cond
                      ;; If `first-b` is a full chunk, just transfer it directly.
                      [(= first-b-len CHUNK-CAPACITY)
                       (set! new-vt (vector-trie-add-first new-vt first-b))
                       (chunk-copy! new-chunk (- CHUNK-CAPACITY last-a-len) last-a 0)
                       last-a-len]
                      [else
                       ;; Otherwise, copy `first-b` into the end of the new chunk.
                       (chunk-copy! new-chunk (- CHUNK-CAPACITY first-b-len) first-b 0)
                       (cond
                         ;; If all of `last-a` fits in the new chunk, just copy it.
                         [(< middle-len CHUNK-CAPACITY)
                          (chunk-copy! new-chunk (- CHUNK-CAPACITY middle-len) last-a 0)
                          middle-len]

                         ;; Otherwise, we have to transfer enough elements to fill the
                         ;; new chunk and spill the leftovers into the next chunk.
                         [else
                          (define transfer-count (- CHUNK-CAPACITY first-b-len))
                          (define leftover-count (- last-a-len transfer-count))
                          (chunk-copy! new-chunk 0 last-a leftover-count)
                          (transfer-chunk!)
                          (chunk-copy! new-chunk (- CHUNK-CAPACITY leftover-count) last-a 0 leftover-count)
                          leftover-count])]))

                  (define insert-i (- CHUNK-CAPACITY split-i))

                  ;; Split and transfer each full chunk in `vt-a`.
                  (for ([full-chunk (in-reversed-vector-trie vt-a)])
                    (chunk-copy! new-chunk 0 full-chunk split-i)
                    (transfer-chunk!)
                    (chunk-copy! new-chunk insert-i full-chunk 0 split-i))

                  ;; Transfer `first-a`.
                  (define first-a-len (chunk-length first-a))
                  (define new-first-c
                    (cond
                      ;; If `first-a` contains too many elements to fit in the next
                      ;; partially-constructed chunk, we need to split it as well.
                      [(> first-a-len insert-i)
                       (chunk-copy! new-chunk 0 first-a split-i)
                       (transfer-chunk! #:done? #t)
                       (chunk-slice first-a 0 split-i)]

                      ;; Otherwise, we can move the elements from the partially-
                      ;; constructed chunk into the new first chunk.
                      [else
                       (make-chunk
                        (+ first-a-len split-i)
                        (λ (new-first-c)
                          (chunk-copy! new-first-c 0 first-a 0)
                          (chunk-copy! new-first-c first-a-len new-chunk insert-i)))]))

                  ;; All done: package the results and return.
                  (chunks new-len new-first-c new-vt last-b)]

                 [else
                  ;; Append case: transfer chunks from `vt-b` into `vt-a`.
                  (define new-vt vt-a)
                  (define new-chunk (make-mutable-chunk CHUNK-CAPACITY))
                  (define (transfer-chunk! #:done? [done? #f])
                    (set! new-vt (vector-trie-add-last new-vt (unsafe-chunk->immutable-chunk! new-chunk)))
                    (set! new-chunk (if done? #f (make-mutable-chunk CHUNK-CAPACITY))))

                  ;; Transfer `last-a` and `first-b` and compute where to split
                  ;; each full chunk in `vt-b`.
                  (define insert-i
                    (cond
                      ;; If `last-a` is a full chunk, just transfer it directly.
                      [(= last-a-len CHUNK-CAPACITY)
                       (set! new-vt (vector-trie-add-last new-vt last-a))
                       (chunk-copy! new-chunk 0 first-b 0)
                       first-b-len]
                      [else
                       ;; Otherwise, copy `last-a` into the start of the new chunk.
                       (chunk-copy! new-chunk 0 last-a 0)
                       (cond
                         ;; If all of `first-b` fits in the new chunk, just copy it.
                         [(< middle-len CHUNK-CAPACITY)
                          (chunk-copy! new-chunk last-a-len first-b 0)
                          middle-len]

                         ;; Otherwise, we have to transfer enough elements to fill the
                         ;; new chunk and spill the leftovers into the next chunk.
                         [else
                          (define transfer-count (- CHUNK-CAPACITY last-a-len))
                          (define leftover-count (- first-b-len transfer-count))
                          (chunk-copy! new-chunk last-a-len first-b 0 transfer-count)
                          (transfer-chunk!)
                          (chunk-copy! new-chunk 0 first-b transfer-count)
                          leftover-count])]))

                  (define split-i (- CHUNK-CAPACITY insert-i))

                  ;; Split and transfer each full chunk in `vt-b`.
                  (for ([full-chunk (in-vector-trie vt-b)])
                    (chunk-copy! new-chunk insert-i full-chunk 0 split-i)
                    (transfer-chunk!)
                    (chunk-copy! new-chunk 0 full-chunk split-i))

                  ;; Transfer `last-b`.
                  (define last-b-len (chunk-length last-b))
                  (define new-last-c
                    (cond
                      ;; If `last-b` contains too many elements to fit in the next
                      ;; partially-constructed chunk, we need to split it as well.
                      [(> last-b-len split-i)
                       (chunk-copy! new-chunk insert-i last-b 0 split-i)
                       (transfer-chunk! #:done? #t)
                       (chunk-slice last-b split-i)]

                      ;; Otherwise, we can move the elements from the partially-
                      ;; constructed chunk into the new last chunk.
                      [else
                       (make-chunk
                        (+ insert-i last-b-len)
                        (λ (new-last-c)
                          (chunk-copy! new-last-c 0 new-chunk 0 insert-i)
                          (chunk-copy! new-last-c insert-i last-b 0)))]))

                  ;; All done: package the results and return.
                  (chunks new-len first-a new-vt new-last-c)])])]

           [(cs-a . cs-bs)
            (for/fold ([cs-a cs-a])
                      ([cs-b (in-list cs-bs)])
              (chunked-seq-append cs-a cs-b))]))

       ;; ----------------------------------------------------------------------
       ;; comparison

       (define (chunked-seq=?/recur cs-a cs-b recur)
         (and (= (chunked-seq-length cs-a)
                 (chunked-seq-length cs-b))
              (for/and ([val-a (-in-chunked-seq cs-a)]
                        [val-b (-in-chunked-seq cs-b)])
                (recur val-a val-b))
              #t))

       (define (chunked-seq-compare/recur cs-a cs-b recur)
         (match* {cs-a cs-b}
           [{(? chunked-seq-empty?) (? chunked-seq-empty?)} '=]
           [{(? chunked-seq-empty?) _                     } '<]
           [{_                      (? chunked-seq-empty?)} '>]
           [{_ _}
            (define-values [more-a? get-a] (sequence-generate (in-chunked-seq-chunks cs-a)))
            (define-values [more-b? get-b] (sequence-generate (in-chunked-seq-chunks cs-b)))

            (define (advance more? get chunk index len)
              (let ([index (add1 index)])
                (if (= index len)
                    (if (more?)
                        (let ([chunk (get)])
                          (values chunk 0 (chunk-length chunk)))
                        (values #f #f #f))
                    (values chunk index len))))

            (define first-a (get-a))
            (define first-b (get-b))
            (let loop ([chunk-a first-a]
                       [index-a 0]
                       [len-a (chunk-length first-a)]
                       [chunk-b first-b]
                       [index-b 0]
                       [len-b (chunk-length first-b)])
              (match (recur (chunk-ref chunk-a index-a) (chunk-ref chunk-b index-b))
                ['< '<]
                ['> '>]
                ['= (let-values ([(chunk-a index-a len-a) (advance more-a? get-a chunk-a index-a len-a)]
                                 [(chunk-b index-b len-b) (advance more-b? get-b chunk-b index-b len-b)])
                      (match* {chunk-a chunk-b}
                        [{#f #f} '=]
                        [{#f _ } '<]
                        [{_  #f} '>]
                        [{_  _ } (loop chunk-a index-a len-a
                                       chunk-b index-b len-b)]))]))]))

       ;; ----------------------------------------------------------------------
       ;; iteration

       (define (in-chunked-seq-chunks cs)
         (unless (chunked-seq? cs)
           (raise-argument-error 'in-chunked-seq-chunks (symbol->string 'chunked-seq?) cs))
         (match cs
           [(? chunked-seq-empty?)
            empty-sequence]
           [(single-chunk chunk)
            (in-value chunk)]
           [(chunks _ first-c vt last-c)
            (in-sequences (in-value first-c)
                          (in-vector-trie vt)
                          (in-value last-c))]))

       ;; Extracted out of `-in-chunked-seq-chunks` to avoid code size bloat.
       (define (initialize-chunked-seq-chunks-iteration cs)
         (match cs
           [(? chunked-seq-empty?)
            (values #f #f 0 #f)]
           [(single-chunk chunk)
            (values chunk #f 0 #f)]
           [(chunks _ first-c vt last-c)
            (values first-c vt (vector-trie-length vt) last-c)]))

       (define-sequence-syntax -in-chunked-seq-chunks
         (λ () #'in-chunked-seq-chunks)
         (syntax-parser
           [[(x:id) (_ {~var cs-e (expr/c #'chunked-seq?)})]
            #'[(x) (:do-in
                    ([(first-c vt vt-len last-c)
                      (initialize-chunked-seq-chunks-iteration cs-e.c)])
                    (void)
                    ([next-vt-i 0] ; #f once `vt` is exhausted
                     [chunk first-c])
                    chunk
                    ([(x) chunk]
                     [(next-vt-i* chunk*)
                      (cond
                        [(not next-vt-i)
                         (values #f #f)]
                        [(= next-vt-i vt-len)
                         (values #f last-c)]
                        [else
                         (values (add1 next-vt-i)
                                 (vector-trie-ref vt next-vt-i))])])
                    #t
                    chunk
                    [next-vt-i* chunk*])]]
           [_ #f]))

       (define (in-chunked-seq cs)
         (unless (chunked-seq? cs)
           (raise-argument-error 'in-chunked-seq (symbol->string 'chunked-seq?) cs))
         (match cs
           [(? chunked-seq-empty?)
            empty-sequence]
           [(single-chunk chunk)
            (in-chunk chunk)]
           [(chunks _ first-c vt last-c)
            (define vt-len (vector-trie-length vt))
            (make-do-sequence
             (λ ()
               (define next-vt-i 0) ; #f once `vt` is exhausted
               (define chunk first-c)
               (define chunk-len (chunk-length first-c))
               (values
                (λ (chunk-i) (chunk-ref chunk chunk-i))
                #f
                (λ (chunk-i)
                  (cond
                    [(< chunk-i (sub1 chunk-len))
                     (add1 chunk-i)]
                    [(not next-vt-i)
                     #f]
                    [(= next-vt-i vt-len)
                     (set! next-vt-i #f)
                     (set! chunk last-c)
                     (set! chunk-len (chunk-length last-c))
                     0]
                    [else
                     (set! chunk (vector-trie-ref vt next-vt-i))
                     (set! next-vt-i (add1 next-vt-i))
                     (set! chunk-len (chunk-length chunk))
                     0]))
                0
                (λ (chunk-i) chunk-i)
                #f
                #f)))]))

       ;; Extracted out of `-in-chunked-seq` to avoid code size bloat.
       (define (initialize-chunked-seq-iteration cs)
         (define-values [first-c vt last-c]
           (match cs
             [(? chunked-seq-empty?)
              (values #f #f #f)]
             [(single-chunk chunk)
              (values chunk #f #f)]
             [(chunks _ first-c vt last-c)
              (values first-c vt last-c)]))

         (values first-c
                 (if first-c (chunk-length first-c) 0)
                 vt
                 (if vt (vector-trie-length vt) 0)
                 last-c
                 (if last-c (chunk-length last-c) 0)))

       (define-sequence-syntax -in-chunked-seq
         (λ () #'in-chunked-seq)
         (syntax-parser
           [[(x:id) (_ {~var cs-e (expr/c #'chunked-seq?)})]
            #'[(x) (:do-in
                    ([(first-c first-c-len vt vt-len last-c last-c-len)
                      (initialize-chunked-seq-iteration cs-e.c)])
                    (void)
                    ([next-vt-i 0] ; #f once `vt` is exhausted
                     [chunk first-c]
                     [chunk-len first-c-len]
                     [chunk-i 0])
                    chunk
                    ([(x) (chunk-ref chunk chunk-i)]
                     [(next-vt-i* chunk* chunk-len* chunk-i*)
                      (cond
                        [(< chunk-i (sub1 chunk-len))
                         (values next-vt-i chunk chunk-len (add1 chunk-i))]
                        [(not next-vt-i)
                         (values #f #f 0 0)]
                        [(= next-vt-i vt-len)
                         (values #f last-c last-c-len 0)]
                        [else
                         (define chunk* (vector-trie-ref vt next-vt-i))
                         (values (add1 next-vt-i)
                                 chunk*
                                 (chunk-length chunk*)
                                 0)])])
                    #t
                    chunk
                    [next-vt-i* chunk* chunk-len* chunk-i*])]]
           [_ #f]))

       ;; ----------------------------------------------------------------------
       ;; conversion

       (define (chunk->chunked-seq big-chunk)
         (define len (chunk-length big-chunk))
         (cond
           [(zero? len)
            empty-chunked-seq]
           [(<= len CHUNK-CAPACITY)
            (single-chunk big-chunk)]
           [else
            (define-values [full-chunk-count leftover-count] (quotient/remainder len CHUNK-CAPACITY))
            (define vt-len (- full-chunk-count (if (zero? leftover-count) 2 1)))
            (chunks len
                    (chunk-slice big-chunk 0 CHUNK-CAPACITY)
                    (build-vector-trie
                     vt-len
                     (λ (chunk-i)
                       (define elem-i (* (add1 chunk-i) CHUNK-CAPACITY))
                       (chunk-slice big-chunk elem-i (+ elem-i CHUNK-CAPACITY))))
                    (chunk-slice big-chunk (* (add1 vt-len) CHUNK-CAPACITY)))]))

       (define (chunked-seq->chunk cs)
         (define len (chunked-seq-length cs))
         (make-chunk
          len
          (λ (big-chunk)
            (define i 0)
            (for ([chunk (-in-chunked-seq-chunks cs)])
              (chunk-copy! big-chunk i chunk 0)
              (set! i (+ i (chunk-length chunk))))))))])

(define-chunked-sequence chunked-list
  #:element-contract any/c
  #:chunk-type vector
  #:chunk-bits 5 ; 32-element chunks
  #:chunk-immutable! unsafe-vector*->immutable-vector!)

(define-chunked-sequence chunked-string
  #:element-contract char?
  #:chunk-type string
  #:chunk-bits 8 ; 256-character chunks
  #:chunk-immutable! unsafe-string->immutable-string!)

(define-chunked-sequence chunked-bytes
  #:element-contract byte?
  #:chunk-type bytes
  #:chunk-bits 8 ; 256-byte chunks
  #:chunk-immutable! unsafe-bytes->immutable-bytes!)
