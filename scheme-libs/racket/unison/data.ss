;; Helpers for building data that conform to the compiler calling convention

#!racket/base
(provide
  make-data
  data
  unison-data?
  unison-data-ref
  unison-data-tag
  unison-data-fields

  declare-unison-data-hash
  data-hash->number
  data-number->hash

  make-sum
  sum
  unison-sum?
  unison-sum-tag
  unison-sum-fields

  make-pure
  unison-pure?
  unison-pure-val

  make-request
  unison-request?
  unison-request-ability
  unison-request-tag
  unison-request-fields

  partial-app
  unison-closure?
  unison-closure-code
  unison-closure-env

  unison-termlink
  unison-termlink?
  unison-termlink-con
  unison-termlink-con?
  unison-termlink-con-ref
  unison-termlink-con-index
  unison-termlink-ref
  unison-termlink-ref?
  unison-termlink-ref-ref

  unison-typelink
  unison-typelink?
  unison-typelink-builtin
  unison-typelink-builtin?
  unison-typelink-builtin-name
  unison-typelink-derived
  unison-typelink-derived?
  unison-typelink-derived-ref

  some
  none
  some?
  none?
  option-get
  right
  left
  right?
  left?
  either-get
  either-get
  unit
  false
  true
  bool
  char
  ord
  any
  failure
  exception
  exn:bug
  make-exn:bug
  exn:bug?
  exn:bug->exception

  unison-tuple->list)

(require
  racket/base
  racket/fixnum
  (only-in "vector-trie.rkt" ->fx/wraparound))

(struct unison-data
  (ref tag fields)
  #:sealed
  #:transparent
  #:constructor-name make-data
  #:property prop:equal+hash
  (let ()
    (define (equal-proc data-l data-r rec)
      (and
        (= (unison-data-tag data-l) (unison-data-tag data-r))
        (andmap rec
                (unison-data-fields data-l)
                (unison-data-fields data-r))))

    (define ((hash-proc init) d rec)
      (for/fold ([hc init])
                ([v (unison-data-fields d)])
        (fxxor (fx*/wraparound hc 31)
               (->fx/wraparound (rec v)))))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(define (data r t . args) (make-data r t args))

(struct unison-sum
  (tag fields)
  #:constructor-name make-sum)

(define (sum t . args) (make-sum t args))

(struct unison-pure
  (val)
  #:constructor-name make-pure)

(struct unison-request
  (ability tag fields)
  #:constructor-name make-request)

; Structures for other unison builtins. Originally the plan was
; just to secretly use an in-unison data type representation.
; However, there are generic functions written in scheme that
; do not have access to typing informatiaon, and it becomes
; impossible to distinguish data type values from pseudo-builtins
; using the same representation, while the behavior must be
; different. So, at least, we must wrap the unison data in
; something that allows us to distinguish it as builtin.
(struct unison-termlink ()
  #:transparent
  #:reflection-name 'termlink)

(struct unison-termlink-con unison-termlink
  (ref index)
  #:reflection-name 'termlink)

(struct unison-termlink-ref unison-termlink
  (ref)
  #:reflection-name 'termlink)

(struct unison-typelink ()
  #:transparent
  #:reflection-name 'typelink)

(struct unison-typelink-builtin unison-typelink
  (name)
  #:reflection-name 'typelink)

(struct unison-typelink-derived unison-typelink
  (ref)
  #:reflection-name 'typelink)

(struct unison-closure
  (code env)
  #:transparent
  #:property prop:procedure
  (case-lambda
    [(clo) clo]
    [(clo . rest)
     (apply (unison-closure-code clo)
            (append (unison-closure-env clo) rest))]))

(define (partial-app f . args) (unison-closure f args))

; Option a
(define none (sum 0))

; a -> Option a
(define (some a) (sum 1 a))

; Option a -> Bool
(define (some? option) (eq? 1 (unison-sum-tag option)))

; Option a -> Bool
(define (none? option) (eq? 0 (unison-sum-tag option)))

; Option a -> a (or #f)
(define (option-get option)
  (if
   (some? option)
   (car (unison-sum-fields option))
   (raise "Cannot get the value of an empty option ")))

; #<void> works as well
; Unit
(define unit (sum 0))

; Booleans are represented as numbers
(define false 0)
(define true 1)

(define (bool b) (if b 1 0))

(define (char c) (char->integer c))

(define (ord o)
  (cond
    [(eq? o '<) 0]
    [(eq? o '=) 1]
    [(eq? o '>) 2]))

; a -> Either b a
(define (right a) (sum 1 a))

; b -> Either b a
(define (left b) (sum 0 b))

; Either a b -> Boolean
(define (right? either) (eq? 1 (unison-sum-tag either)))

; Either a b -> Boolean
(define (left? either) (eq? 0 (unison-sum-tag either)))

; Either a b -> a | b
(define (either-get either) (car (unison-sum-fields either)))

; a -> Any
(define (any a) (data 'Any 0 a))

; Type -> Text -> Any -> Failure
(define (failure typeLink msg any)
  (sum 0 typeLink msg any))

; Type -> Text -> a ->{Exception} b
(define (exception typeLink msg a)
  (failure typeLink msg (any a)))

; TODO needs better pretty printing for when it isn't caught
(struct exn:bug (msg a)
  #:constructor-name make-exn:bug)
(define (exn:bug->exception b) (exception "RuntimeFailure" (exn:bug-msg b) (exn:bug-a b)))


; A counter for internally numbering declared data, so that the
; entire reference doesn't need to be stored in every data record.
(define next-data-number 0)
(define (fresh-data-number)
  (let ([n next-data-number])
    (set! next-data-number (+ n 1))
    n))

; maps hashes of declared unison data to their internal numberings
(define data-hash-numberings (make-hash))

; maps internal numberings of declared unison data to their hashes
(define data-number-hashes (make-hash))

; Adds a hash to the known set of data types, allocating an
; internal numbe for it.
(define (declare-unison-data-hash bs)
  (let ([n (fresh-data-number)])
    (hash-set! data-hash-numberings bs n)
    (hash-set! data-number-hashes n bs)))

(define (data-hash->number bs)
  (hash-ref data-hash-numberings bs))

(define (data-number->hash n)
  (hash-ref data-number-hashes n))

(define (unison-tuple->list t)
  (let ([fs (unison-data-fields t)])
    (cond
      [(null? fs) '()]
      [(= 2 (length fs))
       (cons (car fs) (unison-tuple->list (cadr fs)))]
      [else
        (raise "unison-tuple->list: unexpected value")])))
