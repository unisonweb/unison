#lang racket/base

(require rackunit
         "../chunked-seq.rkt"
         "../string-search.rkt")

(define (cb v) (bytes->chunked-bytes v))
(define (cs v) (string->chunked-string v))

(test-case
 "chunked-bytes-index-of finds all substrings"
 (define bs #"abcdefghi")
 (define bs-len (bytes-length bs))
 (for* ([i (in-range bs-len)]
        [len (in-inclusive-range 1 (- bs-len i))])
   (with-check-info (['i i] ['len len])
     (check-equal? (chunked-bytes-index-of
                    (cb bs)
                    (cb (subbytes bs i (+ i len))))
                   i))))

(test-case
 "chunked-string-index-of finds all substrings"
 (define str "abcdefghi")
 (define str-len (string-length str))
 (for* ([i (in-range str-len)]
        [len (in-inclusive-range 1 (- str-len i))])
   (with-check-info (['i i] ['len len])
     (check-equal? (chunked-string-index-of
                    (cs str)
                    (cs (substring str i (+ i len))))
                   i))))

(check-equal? (chunked-string-index-of (cs "hello") (cs "goodbye")) #f)
(check-equal? (chunked-string-index-of (cs "hello") (cs "l")) 2)
(check-equal? (chunked-string-index-of (cs "hello") (cs "lo")) 3)
(check-equal? (chunked-string-index-of (cs "1needle 2needle 3needle 4needle") (cs "3needle")) 16)

(define (random-bytes len)
  (define bs (make-bytes len))
  (for ([i (in-range len)])
    (bytes-set! bs i (random 256)))
  bs)

(test-case
 "chunked-bytes-index-of finds random needles"
 (for ([i (in-range 100)])
   (define haystack-len (random 1000 10000))
   (define haystack (random-bytes haystack-len))

   (define needle-start (random haystack-len))
   (define needle-end (random needle-start (add1 haystack-len)))
   (define needle (subbytes haystack needle-start needle-end))

   (with-check-info (['haystack haystack]
                     ['needle needle])
     (check-equal? (chunked-bytes-index-of (cb haystack) (cb needle))
                   (caar (regexp-match-positions (regexp-quote needle) haystack))))))

(define (random-char)
  (define n (random #x10800))
  (integer->char
   (if (< n #xD800)
       n
       (+ n #x800))))

(define (random-string len)
  (define str (make-string len))
  (for ([i (in-range len)])
    (string-set! str i (random-char)))
  str)

(test-case
 "chunked-string-index-of finds random needles"
 (for ([i (in-range 100)])
   (define haystack-len (random 1000 10000))
   (define haystack (random-string haystack-len))

   (define needle-start (random haystack-len))
   (define needle-end (random needle-start (add1 haystack-len)))
   (define needle (substring haystack needle-start needle-end))

   (with-check-info (['haystack haystack]
                     ['needle needle])
     (check-equal? (chunked-string-index-of (cs haystack) (cs needle))
                   (caar (regexp-match-positions (regexp-quote needle) haystack))))))
