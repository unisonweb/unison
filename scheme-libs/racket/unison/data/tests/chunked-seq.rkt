#lang racket/base

(require data/order
         rackunit
         "../chunked-seq.rkt")

(check-pred chunked-list-empty? empty-chunked-list)
(check-equal? (chunked-list-length empty-chunked-list) 0)
(check-equal? (vector->chunked-list #()) empty-chunked-list)
(check-equal? (chunked-list->vector empty-chunked-list) #())

(check-equal? (vector->chunked-list #(1 2 3)) (vector->chunked-list #(1 2 3)))
(check-not-equal? (vector->chunked-list #(1 2)) (vector->chunked-list #(1 2 3)))
(check-not-equal? (vector->chunked-list #(1 2 3)) (vector->chunked-list #(a b c)))

(define (in-test-sizes)
  (in-sequences (in-range 3000)
                (in-range 3000 10000 1000)
                (in-range 10000 100000 10000)))

(test-case
 "chunked-seq-length ∘ make-chunked-seq ≡ id"
 (for ([len (in-test-sizes)])
   (with-check-info (['len len])
     (check-equal? (chunked-list-length (make-chunked-list len #f)) len))))

(test-case
 "chunked-seq-length ∘ chunk->chunked-seq ≡ chunk-length"
 (for ([len (in-test-sizes)])
   (with-check-info (['len len])
     (check-equal? (chunked-list-length (vector->chunked-list (make-vector len))) len))))

(test-case
 "chunked-seq->chunk ∘ chunk->chunked-seq ≡ id"
 (for ([len (in-test-sizes)])
   (define vec (build-vector len number->string))
   (with-check-info (['len len])
     (check-equal? (chunked-list->vector (vector->chunked-list vec)) vec))))

(test-case
 "chunk->chunked-seq ∘ build-chunk ≡ build-chunked-seq"
 (for ([len (in-test-sizes)])
   (with-check-info (['len len])
     (check-equal? (vector->chunked-list (build-vector len number->string))
                   (build-chunked-list len number->string)))))

(test-case
 "chunked-seq-ref ∘ chunk->chunked-seq ≡ chunk-ref"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (define vec (build-vector len number->string))
   (define chunked (vector->chunked-list vec))
   (for ([i (in-range len)])
     (with-check-info (['len len]
                       ['index i])
       (check-equal? (chunked-list-ref chunked i)
                     (vector-ref vec i))))))

(test-case
 "chunked-seq-ref ∘ chunked-seq-set ≡ id"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (define chunked (make-chunked-list len #f))
   (for ([i (in-range len)])
     (with-check-info (['len len]
                       ['index i])
       (define val (number->string i))
       (check-equal? (chunked-list-ref (chunked-list-set chunked i val) i) val)))))

(test-case
 "fold chunked-seq-add-first ≡ build-chunked-seq"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (with-check-info (['len len])
     (check-equal? (for/fold ([lst empty-chunked-list])
                             ([i (in-inclusive-range (sub1 len) 0 -1)])
                     (chunked-list-add-first lst (number->string i)))
                   (build-chunked-list len number->string)))))

(test-case
 "fold chunked-seq-add-last ≡ build-chunked-seq"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (with-check-info (['len len])
     (check-equal? (for/fold ([lst empty-chunked-list])
                             ([i (in-range len)])
                     (chunked-list-add-last lst (number->string i)))
                   (build-chunked-list len number->string)))))

(test-case
 "chunked-seq-drop-first ∘ chunked-seq-add-first ≡ id"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (define lst (build-chunked-list len number->string))
   (with-check-info (['len len])
     (check-equal? (chunked-list-drop-first (chunked-list-add-first lst 'thing)) lst))))

(test-case
 "chunked-seq-drop-last ∘ chunked-seq-add-last ≡ id"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (define lst (build-chunked-list len number->string))
   (with-check-info (['len len])
     (check-equal? (chunked-list-drop-last (chunked-list-add-last lst 'thing)) lst))))

(test-case
 "chunked-seq-append / build-chunked-seq"
 (define (go len-a len-b)
   (with-check-info (['len-a len-a]
                     ['len-b len-b])
     (define lst-a (build-chunked-list len-a number->string))
     (define lst-b (build-chunked-list len-b (λ (i) (number->string (+ len-a i)))))
     (define lst-c (build-chunked-list (+ len-a len-b) number->string))
     (check-equal? (chunked-list-append lst-a lst-b) lst-c)))

 (for* ([len-a (in-range 300)]
        [len-b (in-range 300)])
   (go len-a len-b))
 (for* ([len-a (in-range 300 1000 100)]
        [len-b (in-range 300 1000 100)])
   (go len-a len-b))
 (for* ([len-a (in-range 1000 10000 1000)]
        [len-b (in-range 1000 10000 1000)])
   (go len-a len-b)))

(let ()
  (define datum-compare (order-comparator datum-order))
  (define (compare vec-a vec-b)
    (chunked-list-compare/recur (vector->chunked-list vec-a) (vector->chunked-list vec-b) datum-compare))

  (check-equal? (compare #() #()) '=)
  (check-equal? (compare #(1) #(1)) '=)
  (check-equal? (compare #() #(1)) '<)
  (check-equal? (compare #(1) #()) '>)
  (check-equal? (compare #(1) #(2)) '<)
  (check-equal? (compare #(2) #(1)) '>))
