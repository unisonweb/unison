
#lang racket

(provide
  unison-curry
  unison-curry-0
  unison-curry-1
  unison-curry-2
  unison-curry-3
  unison-curry-4
  unison-curry-5
  unison-curry-6
  unison-curry-7
  unison-curry-8
  unison-curry-9
  unison-curry-10
  unison-curry-11
  unison-curry-12
  unison-curry-13
  unison-curry-14
  unison-curry-15
  unison-curry-16
  unison-curry-17
  unison-curry-18
  unison-curry-19
  unison-curry-20)

(require racket/performance-hint
         racket/unsafe/undefined
         (for-syntax
           (only-in racket
                    const range match empty-sequence))
         unison/data)

(define-for-syntax (vsym #:pre [pre "x"] n)
  (string->symbol (string-append pre (number->string n))))

(define-for-syntax (curry-cases loc n fun:stx us vs)
  (define (sub us vs) (curry-expr loc n fun:stx us vs))

  (for/foldr ([cases (list)]) ([p (in-partitions vs)])
    (match p
      [(cons pre post)
       (with-syntax ([(u ...) us]
                     [(v ...) pre]
                     [f fun:stx])
         (cond
           [(null? post)
            (list*
              (syntax/loc loc
                [(v ...) (f u ... v ...)])
              (syntax/loc loc
                [(v ... . rest) (apply (f u ... v ...) rest)])
              cases)]
           [else
            (with-syntax ([sc (sub (append us pre) post)])
              (cons
                (syntax/loc loc [(v ...) sc])
                cases))]))])))

; Build case-lambdas that are nested n-deep for partitions of
; variables us and vs.
(define-for-syntax (curry-expr loc n fun:stx us vs)
  (cond
    [(= 0 n)
     (with-syntax ([(u ...) us] [f fun:stx])
       (syntax/loc loc
         (unison-closure f (list u ...))))]
    [else
     (with-syntax ([(c ...) (curry-cases loc (sub1 n) fun:stx us vs)])
       (syntax/loc loc
         (case-lambda c ...)))]))

(define-for-syntax (in-parts pre post)
  (in-sequences
    (in-value (cons (reverse pre) post))
    (match post
      ['() empty-sequence]
      [(cons x xs) (in-parts (cons x pre) xs)])))

(define-for-syntax (in-partitions xs) (in-parts '() xs))

(define-for-syntax (build-curry loc n)
  (define xs:stx (generate-temporaries (map (const 'x) (range n))))
  (define fun:stx (syntax/loc loc f))

  (with-syntax ([body (curry-expr loc 2 fun:stx '() xs:stx)])
    (syntax/loc loc
      (lambda (f) body))))

(define-syntax (make-curry stx)
  (syntax-case stx ()
    [(make-curry n)
     (build-curry stx (syntax->datum #'n))]))

(begin-encourage-inline
  (define ((unison-curry-0 f) #:reflect [ref? unsafe-undefined] . rest)
    (if (eq? ref? unsafe-undefined)
      (if (= (length rest) 0)
        (f)
        (apply (f) rest))
      (unison-closure f rest)))

  (define unison-curry-1 (make-curry 1))
  (define unison-curry-2 (make-curry 2))
  (define unison-curry-3 (make-curry 3))
  (define unison-curry-4 (make-curry 4))
  (define unison-curry-5 (make-curry 5))
  (define unison-curry-6 (make-curry 6))
  (define unison-curry-7 (make-curry 7))
  (define unison-curry-8 (make-curry 8))
  (define unison-curry-9 (make-curry 9))
  (define unison-curry-10 (make-curry 10))
  (define unison-curry-11 (make-curry 11))
  (define unison-curry-12 (make-curry 12))
  (define unison-curry-13 (make-curry 13))
  (define unison-curry-14 (make-curry 14))
  (define unison-curry-15 (make-curry 15))
  (define unison-curry-16 (make-curry 16))
  (define unison-curry-17 (make-curry 17))
  (define unison-curry-18 (make-curry 18))
  (define unison-curry-19 (make-curry 19))
  (define unison-curry-20 (make-curry 20)))

(define-syntax (unison-curry stx)
  (syntax-case stx ()
    [(unison-curry n f)
     (begin
       (define m (syntax->datum #'n))
       (define curry:stx (vsym #:pre "unison-curry-" m))
       (with-syntax ([u-curry curry:stx])
         (syntax/loc stx
           (u-curry f))))]))


