#lang racket/base

(require unison/boot
         unison/data
         unison/data-info)

(provide
  builtin-Universal.==
  builtin-Universal.==:termlink
  builtin-Universal.>
  builtin-Universal.>:termlink
  builtin-Universal.>=
  builtin-Universal.>=:termlink
  builtin-Universal.<
  builtin-Universal.<:termlink
  builtin-Universal.<=
  builtin-Universal.<=:termlink
  builtin-Universal.compare
  builtin-Universal.compare:termlink)


(define-unison-builtin (builtin-Universal.== x y) (universal=? x y))

(define-unison-builtin (builtin-Universal.> x y)
  (case (universal-compare x y) [(>) #t] [else #f]))

(define-unison-builtin (builtin-Universal.< x y)
  (case (universal-compare x y) [(<) #t] [else #f]))

(define-unison-builtin (builtin-Universal.<= x y)
  (case (universal-compare x y) [(>) #f] [else #t]))

(define-unison-builtin (builtin-Universal.>= x y)
  (case (universal-compare x y) [(<) #f] [else #t]))

(define-unison-builtin (builtin-Universal.compare x y)
  (case (universal-compare x y)
    [(>) 1] [(<) -1] [else 0]))

