;; Helpers for building data that conform to the compiler calling convention

#!r6rs
(library (unison data)
  (export

   make-data
   data
   data?
   data-ref
   data-tag
   data-fields

   make-sum
   sum
   sum?
   sum-tag
   sum-fields

   make-pure
   pure?
   pure-val

   make-request
   request?
   request-ability
   request-tag
   request-fields

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
   ord
   any
   failure
   exception
   exn:bug
   make-exn:bug
   exn:bug?
   exn:bug->exception)

  (import (rnrs))

  (define-record-type (unison-data make-data data?)
    (fields
      (immutable ref data-ref)
      (immutable tag data-tag)
      (immutable fields data-fields)))

  (define (data r t . args) (make-data r t args))

  (define-record-type (unison-sum make-sum sum?)
    (fields
      (immutable tag sum-tag)
      (immutable fields sum-fields)))

  (define (sum t . args) (make-sum t args))

  (define-record-type (unison-pure make-pure pure?)
    (fields
      (immutable val pure-val)))

  (define-record-type (unison-request make-request request?)
    (fields
      (immutable ability request-ability)
      (immutable tag request-tag)
      (immutable fields request-fields)))

  ; Option a
  (define none (sum 0))

  ; a -> Option a
  (define (some a) (sum 1 a))

  ; Option a -> Bool
  (define (some? option) (eq? 1 (sum-tag option)))

  ; Option a -> Bool
  (define (none? option) (eq? 0 (sum-tag option)))

  ; Option a -> a (or #f)
  (define (option-get option)
    (if
     (some? option)
     (car (sum-fields option))
     (raise "Cannot get the value of an empty option ")))

  ; #<void> works as well
  ; Unit
  (define unit (sum 0))

  ; Booleans are represented as numbers
  (define false 0)
  (define true 1)

  (define (bool b) (if b 1 0))

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
  (define (right? either) (eq? 1 (sum-tag either)))

  ; Either a b -> Boolean
  (define (left? either) (eq? 0 (sum-tag either)))

  ; Either a b -> a | b
  (define (either-get either) (car (sum-fields either)))

  ; a -> Any
  (define (any a) (data 'Any 0 a))

  ; Type -> Text -> Any -> Failure
  (define (failure typeLink msg any)
    (sum 0 typeLink msg any))

  ; Type -> Text -> a ->{Exception} b
  (define (exception typeLink msg a)
    (failure typeLink msg (any a)))

  ; TODO needs better pretty printing for when it isn't caught
  (define-record-type exn:bug (fields msg a))
  (define (exn:bug->exception b) (exception "RuntimeFailure" (exn:bug-msg b) (exn:bug-a b))))

