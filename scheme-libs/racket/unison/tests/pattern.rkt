#lang racket/base

(require rackunit
         "../pattern.rkt"
         "../data/chunked-seq.rkt")

(define (cs v) (string->chunked-string v))
(define (ok rest [captures '()])
  (cons (cs rest) (map cs captures)))

(check-equal? (pattern-match eof (cs "")) (cons (cs "") '()))
(check-equal? (pattern-match eof (cs "a")) #f)

(check-equal? (pattern-match any-char (cs "")) #f)
(check-equal? (pattern-match any-char (cs "a")) (ok ""))
(check-equal? (pattern-match any-char (cs "ab")) (ok "b"))
(check-equal? (pattern-match any-char (cs "abc")) (ok "bc"))

(check-equal? (pattern-match digit (cs "a")) #f)
(check-equal? (pattern-match digit (cs "1")) (ok ""))
(check-equal? (pattern-match digit (cs "25")) (ok "5"))
(check-equal? (pattern-match digit (cs "६")) #f)
(check-equal? (pattern-match digit (cs "¹")) #f)

(check-equal? (pattern-match letter (cs "1")) #f)
(check-equal? (pattern-match letter (cs "a")) (ok ""))
(check-equal? (pattern-match letter (cs "π")) (ok ""))

(check-equal? (pattern-match punctuation (cs "a")) #f)
(check-equal? (pattern-match punctuation (cs ".")) (ok ""))

(check-equal? (pattern-match space (cs "a")) #f)
(check-equal? (pattern-match space (cs " ")) (ok ""))
(check-equal? (pattern-match space (cs "\n")) (ok ""))

(check-equal? (pattern-match (literal (cs "")) (cs "")) (ok ""))
(check-equal? (pattern-match (literal (cs "")) (cs "a")) (ok "a"))
(check-equal? (pattern-match (literal (cs "a")) (cs "")) #f)
(check-equal? (pattern-match (literal (cs "a")) (cs "a")) (ok ""))
(check-equal? (pattern-match (literal (cs "a")) (cs "ab")) (ok "b"))
(check-equal? (pattern-match (literal (cs "ab")) (cs "a")) #f)
(check-equal? (pattern-match (literal (cs "ab")) (cs "ab")) (ok ""))
(check-equal? (pattern-match (literal (cs "ab")) (cs "abc")) (ok "c"))

(let ([pat (chars (cs "abcd"))])
  (check-equal? (pattern-match pat (cs "a")) (ok ""))
  (check-equal? (pattern-match pat (cs "b")) (ok ""))
  (check-equal? (pattern-match pat (cs "c")) (ok ""))
  (check-equal? (pattern-match pat (cs "d")) (ok ""))
  (check-equal? (pattern-match pat (cs "e")) #f)
  (check-equal? (pattern-match pat (cs "abcd")) (ok "bcd")))

(let ([pat (char-range #\a #\d)])
  (check-equal? (pattern-match pat (cs "a")) (ok ""))
  (check-equal? (pattern-match pat (cs "b")) (ok ""))
  (check-equal? (pattern-match pat (cs "c")) (ok ""))
  (check-equal? (pattern-match pat (cs "d")) (ok ""))
  (check-equal? (pattern-match pat (cs "e")) #f)
  (check-equal? (pattern-match pat (cs "abcd")) (ok "bcd")))

(let ([pat (join (literal (cs "ab")) (literal (cs "cd")) eof)])
  (check-equal? (pattern-match pat (cs "ab")) #f)
  (check-equal? (pattern-match pat (cs "abcd")) (ok ""))
  (check-equal? (pattern-match pat (cs "abcde")) #f))

(let ([pat (choice (literal (cs "ab")) (literal (cs "cd")))])
  (check-equal? (pattern-match pat (cs "ab")) (ok ""))
  (check-equal? (pattern-match pat (cs "cd")) (ok ""))
  (check-equal? (pattern-match pat (cs "ef")) #f)
  (check-equal? (pattern-match pat (cs "ad")) #f))

(let ([pat (many (literal (cs "ab")))])
  (check-equal? (pattern-match pat (cs "")) (ok ""))
  (check-equal? (pattern-match pat (cs "ab")) (ok ""))
  (check-equal? (pattern-match pat (cs "abab")) (ok ""))
  (check-equal? (pattern-match pat (cs "ababab")) (ok ""))
  (check-equal? (pattern-match pat (cs "c")) (ok "c"))
  (check-equal? (pattern-match pat (cs "abc")) (ok "c"))
  (check-equal? (pattern-match pat (cs "ababc")) (ok "c"))
  (check-equal? (pattern-match pat (cs "abababc")) (ok "c")))

(let ([pat (replicate (literal (cs "ab")) 3)])
  (check-equal? (pattern-match pat (cs "")) #f)
  (check-equal? (pattern-match pat (cs "ab")) #f)
  (check-equal? (pattern-match pat (cs "abab")) #f)
  (check-equal? (pattern-match pat (cs "ababab")) (ok ""))
  (check-equal? (pattern-match pat (cs "abababab")) (ok "ab")))

(let ([pat (join (capture (many letter)) (capture (many digit)))])
  (check-equal? (pattern-match pat (cs "")) (ok "" '("" "")))
  (check-equal? (pattern-match pat (cs "abc")) (ok "" '("abc" "")))
  (check-equal? (pattern-match pat (cs "123")) (ok "" '("" "123")))
  (check-equal? (pattern-match pat (cs "abc123")) (ok "" '("abc" "123")))
  (check-equal? (pattern-match pat (cs ".")) (ok "." '("" "")))
  (check-equal? (pattern-match pat (cs "abc.")) (ok "." '("abc" "")))
  (check-equal? (pattern-match pat (cs "123.")) (ok "." '("" "123")))
  (check-equal? (pattern-match pat (cs "abc123.")) (ok "." '("abc" "123")))
  (check-equal? (pattern-match pat (cs ".abc123")) (ok ".abc123" '("" ""))))

(let ([pat (many (capture (join letter digit)))])
  (check-equal? (pattern-match pat (cs "")) (ok "" '()))
  (check-equal? (pattern-match pat (cs "a")) (ok "a" '()))
  (check-equal? (pattern-match pat (cs "a1")) (ok "" '("a1")))
  (check-equal? (pattern-match pat (cs "a1b")) (ok "b" '("a1")))
  (check-equal? (pattern-match pat (cs "a1b2")) (ok "" '("a1" "b2")))
  (check-equal? (pattern-match pat (cs "a1b2c")) (ok "c" '("a1" "b2")))
  (check-equal? (pattern-match pat (cs "a1b2c3")) (ok "" '("a1" "b2" "c3"))))

