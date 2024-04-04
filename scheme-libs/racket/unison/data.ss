;; Helpers for building data that conform to the compiler calling convention

#!racket/base
(provide
  declare-unison-data-hash
  data-hash->number
  data-number->hash
  declare-function-link
  lookup-function-link
  declare-code
  lookup-code
  have-code?

  (struct-out unison-data)
  (struct-out unison-sum)
  (struct-out unison-pure)
  (struct-out unison-request)
  (struct-out unison-closure)
  (struct-out unison-termlink)
  (struct-out unison-termlink-con)
  (struct-out unison-termlink-builtin)
  (struct-out unison-termlink-derived)
  (struct-out unison-typelink)
  (struct-out unison-typelink-builtin)
  (struct-out unison-typelink-derived)
  (struct-out unison-code)
  (struct-out unison-quote)
  (struct-out unison-timespec)

  define-builtin-link
  declare-builtin-link

  data
  sum
  partial-app

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
  failure
  exception

  builtin-any:typelink
  unison-any-any:tag
  unison-any-any

  builtin-boolean:typelink
  unison-boolean-true:tag
  unison-boolean-false:tag
  unison-boolean-true
  unison-boolean-false

  builtin-bytes:typelink
  builtin-char:typelink
  builtin-float:typelink
  builtin-int:typelink
  builtin-nat:typelink
  builtin-text:typelink
  builtin-code:typelink
  builtin-mvar:typelink
  builtin-pattern:typelink
  builtin-promise:typelink
  builtin-sequence:typelink
  builtin-socket:typelink
  builtin-tls:typelink
  builtin-timespec:typelink
  builtin-threadid:typelink
  builtin-value:typelink

  builtin-crypto.hashalgorithm:typelink
  builtin-char.class:typelink
  builtin-immutablearray:typelink
  builtin-immutablebytearray:typelink
  builtin-mutablearray:typelink
  builtin-mutablebytearray:typelink
  builtin-processhandle:typelink
  builtin-ref.ticket:typelink
  builtin-tls.cipher:typelink
  builtin-tls.clientconfig:typelink
  builtin-tls.privatekey:typelink
  builtin-tls.serverconfig:typelink
  builtin-tls.signedcert:typelink
  builtin-tls.version:typelink

  unison-tuple->list)

(require
  racket
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
  #:constructor-name make-request
  #:transparent)

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
  #:reflection-name 'termlink
  #:property prop:equal+hash
  (let ()
    (define (equal-proc lnl lnr rec)
      (match lnl
        [(unison-termlink-con r i)
         (match lnr
           [(unison-termlink-con l j)
            (and (rec r l) (= i j))]
           [else #f])]
        [(unison-termlink-builtin l)
         (match lnr
           [(unison-termlink-builtin r)
            (equal? l r)]
           [else #f])]
        [(unison-termlink-derived hl i)
         (match lnr
           [(unison-termlink-derived hr j)
            (and (equal? hl hr) (= i j))]
           [else #f])]))

    (define ((hash-proc init) ln rec)
      (match ln
        [(unison-termlink-con r i)
         (fxxor (fx*/wraparound (rec r) 29)
                (fx*/wraparound (rec i) 23)
                (fx*/wraparound init 17))]
        [(unison-termlink-builtin n)
         (fxxor (fx*/wraparound (rec n) 31)
                (fx*/wraparound init 13))]
        [(unison-termlink-derived hl i)
         (fxxor (fx*/wraparound (rec hl) 37)
                (fx*/wraparound (rec i) 41)
                (fx*/wraparound init 7))]))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(struct unison-termlink-con unison-termlink
  (ref index)
  #:reflection-name 'termlink)

(struct unison-termlink-builtin unison-termlink
  (name)
  #:reflection-name 'termlink)

(struct unison-termlink-derived unison-termlink
  (hash i)
  #:reflection-name 'termlink)

(struct unison-typelink ()
  #:transparent
  #:reflection-name 'typelink
  #:property prop:equal+hash
  (let ()
    (define (equal-proc lnl lnr rec)
      (match lnl
        [(unison-typelink-builtin l)
         (match lnr
           [(unison-typelink-builtin r)
            (equal? l r)]
           [else #f])]
        [(unison-typelink-derived hl i)
         (match lnr
           [(unison-typelink-derived hr j)
            (and (equal? hl hr) (= i j))]
           [else #f])]))

    (define ((hash-proc init) ln rec)
      (match ln
        [(unison-typelink-builtin n)
         (fxxor (fx*/wraparound (rec n) 53)
                (fx*/wraparound init 17))]
        [(unison-typelink-derived hl i)
         (fxxor (fx*/wraparound (rec hl) 59)
                (fx*/wraparound (rec i) 61)
                (fx*/wraparound init 19))]))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(struct unison-typelink-builtin unison-typelink
  (name)
  #:reflection-name 'typelink
  #:transparent)

(struct unison-typelink-derived unison-typelink
  (ref ix)
  #:reflection-name 'typelink
  #:transparent)

(struct unison-code (rep))
(struct unison-quote (val))

(struct unison-closure
  (code env)
  #:transparent
  #:property prop:procedure
  (case-lambda
    [(clo) clo]
    [(clo . rest)
     (apply (unison-closure-code clo)
            (append (unison-closure-env clo) rest))]))

(struct unison-timespec (sec nsec)
  #:transparent
  #:property prop:equal+hash
  (let ()
    (define (equal-proc tml tmr rec)
      (match tml
        [(unison-timespec sl nsl)
         (match tmr
           [(unison-timespec sr nsr)
            (and (= sl sr) (= nsl nsr))])]))

    (define ((hash-proc init) tm rec)
      (match tm
        [(unison-timespec s ns)
         (fxxor (fx*/wraparound (rec s) 67)
                (fx*/wraparound (rec ns) 71)
                (fx*/wraparound init 73))]))

    (list equal-proc (hash-proc 3) (hash-proc 5))))

(define-syntax (define-builtin-link stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     (let* ([sym (syntax-e #'name)]
            [txt (symbol->string sym)]
            [dname (datum->syntax stx
                     (string->symbol
                       (string-append
                         "builtin-" txt ":termlink")))])
       #`(define #,dname
           (unison-termlink-builtin #,(datum->syntax stx txt))))]))

(define-syntax (declare-builtin-link stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     (let* ([sym (syntax-e #'name)]
            [txt (symbol->string sym)]
            [dname (datum->syntax stx
                     (string->symbol
                       (string-append txt ":termlink")))])
       #`(declare-function-link name #,dname))]))

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
(define builtin-any:typelink (unison-typelink-builtin "Any"))
(define unison-any-any:tag 0)
(define (unison-any-any x)
  (data builtin-any:typelink unison-any-any:tag x))

(define builtin-boolean:typelink (unison-typelink-builtin "Boolean"))
(define unison-boolean-true:tag 1)
(define unison-boolean-false:tag 0)
(define unison-boolean-true
  (data builtin-boolean:typelink unison-boolean-true:tag))
(define unison-boolean-false
  (data builtin-boolean:typelink unison-boolean-false:tag))

(define builtin-bytes:typelink (unison-typelink-builtin "Bytes"))
(define builtin-char:typelink (unison-typelink-builtin "Char"))
(define builtin-code:typelink (unison-typelink-builtin "Code"))
(define builtin-float:typelink (unison-typelink-builtin "Float"))
(define builtin-int:typelink (unison-typelink-builtin "Int"))
(define builtin-mvar:typelink (unison-typelink-builtin "MVar"))
(define builtin-nat:typelink (unison-typelink-builtin "Nat"))
(define builtin-pattern:typelink (unison-typelink-builtin "Pattern"))
(define builtin-promise:typelink (unison-typelink-builtin "Promise"))
(define builtin-sequence:typelink (unison-typelink-builtin "Sequence"))
(define builtin-socket:typelink (unison-typelink-builtin "Socket"))
(define builtin-text:typelink (unison-typelink-builtin "Text"))
(define builtin-tls:typelink (unison-typelink-builtin "Tls"))
(define builtin-timespec:typelink (unison-typelink-builtin "TimeSpec"))
(define builtin-threadid:typelink (unison-typelink-builtin "ThreadId"))
(define builtin-value:typelink (unison-typelink-builtin "Value"))

(define builtin-crypto.hashalgorithm:typelink
  (unison-typelink-builtin "crypto.HashAlgorithm"))
(define builtin-char.class:typelink
  (unison-typelink-builtin "Char.Class"))
(define builtin-immutablearray:typelink
  (unison-typelink-builtin "ImmutableArray"))
(define builtin-immutablebytearray:typelink
  (unison-typelink-builtin "ImmutableByteArray"))
(define builtin-mutablearray:typelink
  (unison-typelink-builtin "MutableArray"))
(define builtin-mutablebytearray:typelink
  (unison-typelink-builtin "MutableArray"))
(define builtin-processhandle:typelink
  (unison-typelink-builtin "ProcessHandle"))
(define builtin-ref.ticket:typelink
  (unison-typelink-builtin "Ref.Ticket"))
(define builtin-tls.cipher:typelink
  (unison-typelink-builtin "Tls.Cipher"))
(define builtin-tls.clientconfig:typelink
  (unison-typelink-builtin "Tls.ClientConfig"))
(define builtin-tls.privatekey:typelink
  (unison-typelink-builtin "Tls.PrivateKey"))
(define builtin-tls.serverconfig:typelink
  (unison-typelink-builtin "Tls.ServerConfig"))
(define builtin-tls.signedcert:typelink
  (unison-typelink-builtin "Tls.SignedCert"))
(define builtin-tls.version:typelink
  (unison-typelink-builtin "Tls.Version"))

; Type -> Text -> Any -> Failure
(define (failure typeLink msg any)
  (sum 0 typeLink msg any))

; Type -> Text -> a ->{Exception} b
(define (exception typeLink msg a)
  (failure typeLink msg (unison-any-any a)))

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
; internal number for it.
(define (declare-unison-data-hash bs)
  (let ([n (fresh-data-number)])
    (hash-set! data-hash-numberings bs n)
    (hash-set! data-number-hashes n bs)))

(define (data-hash->number bs)
  (hash-ref data-hash-numberings bs))

(define (data-number->hash n)
  (hash-ref data-number-hashes n))

(define function-associations (make-hash))

(define (declare-function-link f ln)
  (hash-set! function-associations f ln))

(define (lookup-function-link f)
  (hash-ref function-associations f))

(define code-associations (make-hash))

(define (declare-code hs co)
  (hash-set! code-associations hs co))

(define (lookup-code hs)
  (let ([mco (hash-ref code-associations hs #f)])
    (if (eq? mco #f)
      (sum 0)
      (sum 1 mco))))

(define (have-code? hs)
  (hash-has-key? code-associations hs))

(define (unison-tuple->list t)
  (let ([fs (unison-data-fields t)])
    (cond
      [(null? fs) '()]
      [(= 2 (length fs))
       (cons (car fs) (unison-tuple->list (cadr fs)))]
      [else
        (raise "unison-tuple->list: unexpected value")])))
