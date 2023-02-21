#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/contract
         racket/match
         racket/sequence
         racket/unsafe/ops
         syntax/parse/define
         "vector-trie.rkt")

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

   (define/with-syntax chunked-seq? (derived-seq-id "~a?"))
   (define/with-syntax empty-chunked-seq (derived-seq-id "empty-~a"))
   (define/with-syntax chunked-seq-empty? (derived-seq-id "~a-empty?"))
   (define/with-syntax chunked-seq-length (derived-seq-id "~a-length"))

   (define/with-syntax chunked-seq-ref (derived-seq-id "~a-ref"))
   (define/with-syntax chunked-seq-set (derived-seq-id "~a-set"))
   (define/with-syntax chunked-seq-add-first (derived-seq-id "~a-add-first"))
   (define/with-syntax chunked-seq-add-last (derived-seq-id "~a-add-last"))
   (define/with-syntax chunked-seq-drop-first (derived-seq-id "~a-drop-first"))
   (define/with-syntax chunked-seq-drop-last (derived-seq-id "~a-drop-last"))

   (define/with-syntax in-chunked-seq (derived-seq-id "in-~a"))

   #`(begin
       (provide chunked-seq?
                empty-chunked-seq
                (contract-out
                 [chunked-seq-length (-> chunked-seq? exact-nonnegative-integer?)]
                 [chunked-seq-empty? (-> chunked-seq? boolean?)]

                 [chunked-seq-ref (-> chunked-seq? exact-nonnegative-integer? elem/c)]
                 [chunked-seq-set (-> chunked-seq? exact-nonnegative-integer? elem/c chunked-seq?)]
                 [chunked-seq-add-first (-> chunked-seq? elem/c chunked-seq?)]
                 [chunked-seq-add-last (-> chunked-seq? elem/c chunked-seq?)]
                 [chunked-seq-drop-first (-> (and/c chunked-seq? (not/c chunked-seq-empty?)) chunked-seq?)]
                 [chunked-seq-drop-last (-> (and/c chunked-seq? (not/c chunked-seq-empty?)) chunked-seq?)])

                (rename-out
                 [-in-chunked-seq in-chunked-seq]))

       ;; ----------------------------------------------------------------------
       ;; chunk operations

       (define (make-chunk len init-proc)
         (define chunk (make-mutable-chunk len))
         (init-proc chunk)
         (unsafe-chunk->immutable-chunk! chunk))

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

       (define (chunk-drop-first chunk val)
         (make-chunk
          (sub1 (chunk-length chunk))
          (λ (new-chunk)
            (chunk-copy! new-chunk 0 chunk 1))))

       (define (chunk-drop-last chunk val)
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
            (chunk-copy! chunk (sub1 len-a) b 0))))

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

       (define empty-chunked-seq
         (let ()
           (struct empty-chunked-seq () #:authentic)
           (empty-chunked-seq)))

       (define (chunked-seq-empty? v)
         (eq? v empty-chunked-seq))

       (struct single-chunk (chunk)
         #:transparent
         #:reflection-name 'chunked-seq)

       ;; Note [chunks-length invariant]
       ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       ;; We maintain the invariant that a `chunks` structure always
       ;; has more than CHUNK-CAPACITY elements. This means we MUST
       ;; collapse `first-chunk` and `last-chunk` into a `single-chunk`
       ;; if we don’t have enough elements.

       (struct chunks
         (length ; see Note [chunks-length invariant]
          first-chunk
          chunk-trie
          last-chunk)
         #:transparent
         #:reflection-name 'chunked-seq)

       (define (chunked-seq? v)
         (or (chunked-seq-empty? v)
             (single-chunk? v)
             (chunks? v)))

       (define (chunked-seq-length cs)
         (match cs
           [(? chunked-seq-empty?) 0]
           [(single-chunk chunk) (chunk-length chunk)]
           [_ (chunks-length cs)]))

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

       (define (chunked-seq-drop-first cs val)
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

       (define (chunked-seq-drop-last cs val)
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
                   chunks cs-b
                   [length (+ len (chunk-length chunk))]
                   [last-chunk (chunk-append last-c chunk)])]
                 [else
                  (define-values [full-chunk last-c*] (chunk-fill-left last-c chunk))
                  (struct-copy
                   chunks cs-b
                   [length (+ len (chunk-length chunk))]
                   [chunk-trie (vector-trie-add-last vt full-chunk)]
                   [last-chunk first-c*])])]

              [{(chunks len-a first-a vt-a last-a) (chunks len-b first-b vt-b last-b)}
               (define new-len (+ len-a len-b))
               (cond
                 [(= (+ (chunk-length last-a) (chunk-length first-b)) CHUNK-CAPACITY)
                  (chunks new-len
                          first-a
                          (vector-trie-append vt-a vt-b))])
               ])]))

       ;; ----------------------------------------------------------------------
       ;; derived operations

       ;; Could be made more efficient by directly walking the internal
       ;; structure, avoiding repeated traversals.
       (define (in-chunked-seq cs)
         (unless (chunked-seq? cs)
           (raise-argument-error 'in-chunked-seq (symbol->string 'chunked-seq?) cs))
         (sequence-map (λ (i) (chunked-seq-ref cs i)) (in-range (chunked-seq-length cs))))
       (define-sequence-syntax -in-chunked-seq
         (λ () #'in-chunked-seq)
         (syntax-parser
           [[(x:id) (_ {~var cs-e (expr/c #'chunked-seq?)})]
            #'[(x) (:do-in
                    ([(cs cs-len) (let ([cs cs-e.c])
                                    (values cs (chunked-seq-length cs)))])
                    (void)
                    ([i 0])
                    (< i cs-len)
                    ([(x) (chunked-seq-ref cs i)])
                    #t
                    #t
                    [(add1 i)])]]
           [_ #f])))])

(define-chunked-sequence chunked-vector
  #:element-contract any/c
  #:chunk-type vector
  #:chunk-bits 5 ; 32-element chunks
  #:chunk-immutable! unsafe-vector*->immutable-vector!)

(define-chunked-sequence chunked-string
  #:element-contract char?
  #:chunk-type string
  #:chunk-bits 9 ; 512-character chunks
  #:chunk-immutable! unsafe-string->immutable-string!)

(define-chunked-sequence chunked-bytes
  #:element-contract byte?
  #:chunk-type bytes
  #:chunk-bits 9 ; 512-byte chunks
  #:chunk-immutable! unsafe-bytes->immutable-bytes!)
