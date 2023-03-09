#lang racket/base

(require rackunit
         "../vector-trie.rkt")

(check-pred vector-trie-empty? empty-vector-trie)
(check-equal? (vector-trie-length empty-vector-trie) 0)
(check-equal? (vector->vector-trie #()) empty-vector-trie)
(check-equal? (vector-trie->vector empty-vector-trie) #())

(check-equal? (vector->vector-trie #(1 2 3)) (vector->vector-trie #(1 2 3)))
(check-not-equal? (vector->vector-trie #(1 2)) (vector->vector-trie #(1 2 3)))
(check-not-equal? (vector->vector-trie #(1 2 3)) (vector->vector-trie #(a b c)))

(define (in-test-sizes)
  (in-sequences (in-range 3000)
                (in-range 3000 10000 1000)
                (in-range 10000 100000 10000)))

(test-case
 "vector-trie-length ∘ make-vector-trie ≡ id"
 (for ([len (in-test-sizes)])
   (with-check-info (['len len])
     (check-equal? (vector-trie-length (make-vector-trie len #f)) len))))

(test-case
 "vector-trie-length ∘ chunk->vector-trie ≡ chunk-length"
 (for ([len (in-test-sizes)])
   (with-check-info (['len len])
     (check-equal? (vector-trie-length (vector->vector-trie (make-vector len))) len))))

(test-case
 "vector-trie->chunk ∘ chunk->vector-trie ≡ id"
 (for ([len (in-test-sizes)])
   (define vec (build-vector len number->string))
   (with-check-info (['len len])
     (check-equal? (vector-trie->vector (vector->vector-trie vec)) vec))))

(test-case
 "chunk->vector-trie ∘ build-chunk ≡ build-vector-trie"
 (for ([len (in-test-sizes)])
   (with-check-info (['len len])
     (check-equal? (vector->vector-trie (build-vector len number->string))
                   (build-vector-trie len number->string)))))

(test-case
 "vector-trie-ref ∘ chunk->vector-trie ≡ chunk-ref"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (define vec (build-vector len number->string))
   (define chunked (vector->vector-trie vec))
   (for ([i (in-range len)])
     (with-check-info (['len len]
                       ['index i])
       (check-equal? (vector-trie-ref chunked i)
                     (vector-ref vec i))))))

(test-case
 "vector-trie-ref ∘ vector-trie-set ≡ id"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (define chunked (make-vector-trie len #f))
   (for ([i (in-range len)])
     (with-check-info (['len len]
                       ['index i])
       (define val (number->string i))
       (check-equal? (vector-trie-ref (vector-trie-set chunked i val) i) val)))))

(test-case
 "fold vector-trie-add-first ≡ build-vector-trie"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (with-check-info (['len len])
     (check-equal? (for/fold ([lst empty-vector-trie])
                             ([i (in-inclusive-range (sub1 len) 0 -1)])
                     (vector-trie-add-first lst (number->string i)))
                   (build-vector-trie len number->string)))))

(test-case
 "fold vector-trie-add-last ≡ build-vector-trie"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (with-check-info (['len len])
     (check-equal? (for/fold ([lst empty-vector-trie])
                             ([i (in-range len)])
                     (vector-trie-add-last lst (number->string i)))
                   (build-vector-trie len number->string)))))

(test-case
 "vector-trie-drop-first ∘ vector-trie-add-first ≡ id"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (define lst (build-vector-trie len number->string))
   (with-check-info (['len len])
     (check-equal? (vector-trie-drop-first (vector-trie-add-first lst 'thing)) lst))))

(test-case
 "vector-trie-drop-last ∘ vector-trie-add-last ≡ id"
 (for ([len (in-sequences (in-range 300) (in-value 10000))])
   (define lst (build-vector-trie len number->string))
   (with-check-info (['len len])
     (check-equal? (vector-trie-drop-last (vector-trie-add-last lst 'thing)) lst))))

(test-case
 "vector-trie-append / build-vector-trie"
 (define (go len-a len-b)
   (with-check-info (['len-a len-a]
                     ['len-b len-b])
     (define vt-a (build-vector-trie len-a number->string))
     (define vt-b (build-vector-trie len-b (λ (i) (number->string (+ len-a i)))))
     (define vt-c (build-vector-trie (+ len-a len-b) number->string))
     (check-equal? (vector-trie-append vt-a vt-b) vt-c)))

 (for* ([len-a (in-range 300)]
        [len-b (in-range 300)])
   (go len-a len-b))
 (for* ([len-a (in-range 300 1000 100)]
        [len-b (in-range 300 1000 100)])
   (go len-a len-b))
 (for* ([len-a (in-range 1000 10000 1000)]
        [len-b (in-range 1000 10000 1000)])
   (go len-a len-b)))
