; This library implements various functions and macros that are used
; internally to the unison scheme libraries. This provides e.g. a
; measure of abstraction for the particular scheme platform. A useful
; feature of one implementation might need to be implemented on top of
; other features of another, and would go in this library.
;
; This library won't be directly imported by the generated unison
; code, so if some function is needed for those, it should be
; re-exported by (unison boot).
(library (unison core)
  (export
    define-virtual-register
    define-init-registers

    describe-value
    decode-value

    universal-compare

    fx1-
    list-head

    exception->string
    record-case
    fluid-let

    freeze-string!
    string-copy!

    freeze-bytevector!
    freeze-vector!)

  (import (chezscheme))

  ; Wrapper for chez scheme's virtual registers, which are top-level
  ; variables that may perform better than normal variables. They are
  ; limited in number, and referenced by an integer.
  ;
  ; This macro allows the definition of names for the virtual registers,
  ; and keeps track of how many have been declared, so that a static
  ; error is thrown if more are declared than are available.
  ;
  ; Virtual registers are thread local
  (meta define virtual-register-inits '())

  (define-syntax (define-virtual-register stx)
    (syntax-case stx ()
      [(define-virtual-register name init)
       (let ([n (length virtual-register-inits)])
         (with-syntax ([reg (datum->syntax #'define-virtual-register n)])
           (cond
             [(>= n (virtual-register-count))
              (syntax-error stx
                            "Could not allocate a virtual register:")]
             [else
               (set! virtual-register-inits
                 (cons #'init virtual-register-inits))
               #`(define-syntax name
                   (identifier-syntax
                     [id (virtual-register reg)]
                     [(set! id e) (set-virtual-register! reg e)]))])))]))

  (define-syntax (define-init-registers stx)
    (syntax-case stx ()
      [(_ name)
       (with-syntax
         ([(set ...)
           (let rec ([l (reverse virtual-register-inits)]
                     [n 0])
             (cond
               [(null? l) '()]
               [else
                 (with-syntax ([reg (datum->syntax #'name n)]
                               [val (car l)])
                   (cons #'(set-virtual-register! reg val)
                         (rec (cdr l) (+ 1 n))))]))])
         #'(define (name) set ... #t))]))

  ; Recovers the original function name from the partial
  ; application name.
  (define (extract-name i)
    (string->symbol ((i 'code) 'name)))

  (define (describe-value x)
    (let explain ([i (inspect/object x)])
      (case (i 'type)
        [(simple) (i 'value)]
        [(variable) (explain (i 'ref))]
        [(procedure)
         (let explain-args ([j (- (i 'length) 1)] [args '()])
           (if (< j 0)
             (cons (extract-name i) args)
             (explain-args
               (- j 1)
               (cons (explain (i 'ref j)) args))))])))

  ; partial, data, cont, lit
  (define (decode-value x)
    (let reify ([i (inspect/object x)])
      (case (i 'type)
        [(simple) (list 3 (i 'value))]
        [(variable) (reify (i 'ref))]
        [(procedure)
         (let reify-args ([j (- (i 'length) 1)] [args '()])
           (if (< j 0)
             (cons* 0 (extract-name i) args)
             (reify-args
               (- j 1)
               (cons (reify (i 'ref j)) args))))])))

  ; 0 = LT
  ; 1 = EQ
  ; 2 = GT
  (define (universal-compare l r)
    (cond
      [(equal? l r) 1]
      [(and (number? l) (number? r)) (if (< l r) 0 2)]
      [else (raise "universal-compare: unimplemented")]))

  (define (exception->string e)
    (let-values ([(port result) (open-string-output-port)])
                 (display-condition e port)
                 (result)))

  (define (freeze-string! s)
    (($primitive $string-set-immutable!) s)
    s)

  (define (freeze-bytevector! bs)
    (($primitive $bytevector-set-immutable!) bs)
    bs)

  (define (freeze-vector! v)
    (($primitive $vector-set-immutable!) v)
    v)
  )
