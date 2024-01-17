#lang racket/base

(require racket/contract
         "chunked-seq.rkt")

(provide (contract-out
          [chunked-bytes-index-of
           (-> chunked-bytes? chunked-bytes? (or/c exact-nonnegative-integer? #f))]
          [chunked-string-index-of
           (-> chunked-string? chunked-string? (or/c exact-nonnegative-integer? #f))]))

;; -----------------------------------------------------------------------------

(define (chunked-bytes-index-of haystack needle)
  (define needle-len (chunked-bytes-length needle))
  (cond
    [(= needle-len 0) 0]
    [(= needle-len 1)
     (define needle-b (chunked-bytes-ref needle 0))
     (for/or ([(b i) (in-indexed (in-chunked-bytes haystack))]
              #:when (= b needle-b))
       i)]
    [else
     (chunked-seq-index-of haystack needle
                           #:length chunked-bytes-length
                           #:ref chunked-bytes-ref
                           #:elem->integer values
                           #:elem=? =)]))

(define (chunked-string-index-of haystack needle)
  (define needle-len (chunked-string-length needle))
  (cond
    [(= needle-len 0) 0]
    [(= needle-len 1)
     (define needle-c (chunked-string-ref needle 0))
     (for/or ([(c i) (in-indexed (in-chunked-string haystack))]
              #:when (char=? c needle-c))
       i)]
    [else
     (chunked-seq-index-of haystack needle
                           #:length chunked-string-length
                           #:ref chunked-string-ref
                           #:elem->integer char->integer
                           #:elem=? char=?)]))

;; Searches for the first occurrence of a given substring. The algorithm is
;; based on one by Frederik Lundh, which in turn is based on work by Boyer,
;; Moore, Horspool, and Sunday. The full list of references:
;;
;; * Lundh:       https://effbot.org/zone/stringlib.htm
;; * Boyer–Moore: https://doi.org/10.1145/359842.359859
;; * Horspool:    https://doi.org/10.1002/spe.4380100608
;; * Sunday:      https://doi.org/10.1145/79173.79184
;;
;; Lundh provides a good overview and motivation of the approach. The
;; essential idea is a simplification of Boyer–Moore that has poorer worst
;; cases but avoids materializing an alphabet-sized table (which is not
;; practical when the alphabet contains 2^21 letters, as in the case of Unicode
;; code points). In place of the table, Lundh’s algorithm essentially uses a
;; small bloom filter to test whether a given character appears in the needle.
;;
;; This implementation is not particularly optimized. We do faithfully implement
;; the algorithm, so we should get all the asymptotics, but the constant factors
;; are likely rather high.
;;
;; Of particular note is a missed optimization in the implementation of the bloom
;; filter, which is intended to take advantage of bitwise operations on unboxed
;; machine words. Unfortunately, Racket does not currently offer unboxed machine
;; words, so the bitmask computed from the needle might not fit in a fixnum. This
;; will incur a small amount of dynamic allocation and some overhead in the inner
;; loop, but the dynamic allocation remains restricted to the precomputation.
(define (chunked-seq-index-of haystack needle
                              #:length seq-length
                              #:ref seq-ref
                              #:elem->integer elem->integer
                              #:elem=? elem=?)
  (define needle-len (seq-length needle))
  (define needle-last-index (sub1 needle-len))
  (define needle-last (seq-ref needle needle-last-index))

  (define (elem->index c)
    (bitwise-and (elem->integer c) #b111111))
  (define (index-bit c)
    (arithmetic-shift 1 (elem->index c)))

  ;; Before searching the haystack, we precompute two values from the needle:
  ;;
  ;; 1. We compute a 64-bit bitmask to use as a Bloom filter, using the lower
  ;;    6 bits of each element as a hash.
  ;;
  ;; 2. We compute a “bad-character skip” for the final character of the needle.
  ;;    That is, we compute the number of positions we can safely skip when
  ;;    the final character is known to match, but overall match fails.
  (define-values [mask skip]
    (let loop ([i 0]
               [mask 0]
               [skip needle-last-index])
      (cond
        [(< i needle-last-index)
         (define c (seq-ref needle i))
         (loop (add1 i)
               (bitwise-ior mask (index-bit c))
               (if (elem=? c needle-last)
                   (- needle-last-index i)
                   skip))]
        [else
         (values (bitwise-ior mask (index-bit needle-last))
                 skip)])))

  ;; Overapproximation: the Bloom filter means we can have false positives.
  (define (elem-in-needle? c)
    (bitwise-bit-set? mask (elem->index c)))

  (define (linear-match? haystack-offset)
    (for/and ([i (in-range needle-last-index)])
      (elem=? (seq-ref haystack (+ haystack-offset i))
              (seq-ref needle i))))

  (define haystack-len (seq-length haystack))

  (let loop ([i needle-last-index])
    (cond
      [(>= i haystack-len)
       #f]

      [(elem=? (seq-ref haystack i) needle-last)
       (define start-i (- i needle-last-index))
       (cond
         [(linear-match? start-i)
          start-i]
         [else
          (define next-i (add1 i))
          (cond
            [(= next-i haystack-len)
             #f]
            [(elem-in-needle? (seq-ref haystack next-i))
             (loop (+ i skip))]
            [else
             (loop (+ next-i needle-len))])])]

      [else
       (define next-i (add1 i))
       (cond
         [(= next-i haystack-len)
          #f]
         [(elem-in-needle? (seq-ref haystack next-i))
          (loop next-i)]
         [else
          (loop (+ next-i needle-len))])])))
