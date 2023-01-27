; This library implements various syntactic constructs and functions
; that are used in the compilation of unison (intermediate) source to
; scheme. The intent is to provide for writing scheme definitions that
; more directly match the source, so that the compiler doesn't need to
; emit all the code necessary to fix up the difference itself.
;
; Probably the best example of this is the define-unison macro, which
; looks similar to scheme's define, but the function being defined is
; allowed to be under/over applied similar to a unison function. It
; has an 'arity' at which computation happens, but the function
; automatically handles being applied to fewer or more arguments than
; that arity appropriately.
(library (unison boot)
  (export
    name
    define-unison
    func-wrap
    handle
    request
    unison-force
    identity
    record-case)

  (import (chezscheme)
          (unison cont))

  ; Helper function. Turns a list of syntax objects into a list-syntax object.
  (meta define (list->syntax l) #`(#,@l))

  ; Concatenates 
  (meta define fun-sym
    (case-lambda
      [(pfx sfx) (string->symbol (string-append pfx "-" sfx))]
      [(pfx ifx sfx)
       (string->symbol (string-append pfx "-" ifx "-" sfx))]))

  ; Computes a symbol for automatically generated partial application
  ; cases, based on number of arguments applied. The partial
  ; application of `f` is (locally) named `f-partial-N`
  (meta define (partial-symbol name m)
    (fun-sym (symbol->string name) "partial" (number->string m)))

  ; As above, but takes a syntactic object representing the arguments
  ; rather than their count.
  (meta define (partial-name name us)
    (datum->syntax name (syntax->datum name)))

  (define-syntax with-name
    (syntax-rules ()
      [(with-name name e) (let ([name e]) name)]))

  ; Builds partial application cases for unison functions. It seems
  ; most efficient to have a case for each posible under-application.
  (meta define (build-partials name formals)
    (let rec ([us formals] [acc '()])
      (syntax-case us ()
        [() (list->syntax (cons #`[() #,name] acc))]
        [(a ... z)
         (rec #'(a ...)
              (cons
                #`[(a ... z)
                   (with-name
                     #,(partial-name name us)
                     (lambda r (apply #,name a ... z r)))]
                acc))])))

  ; Given an overall function name, a fast path name, and a list of arguments,
  ; builds the case-lambda body of a unison function that enables applying to
  ; arbitrary numbers of arguments.
  (meta define (func-cases name fast args)
    (syntax-case args ()
      [() #`(case-lambda
              [() (#,fast)]
              [r (apply (#,fast) r)])]
      [(a ... z)
       #`(case-lambda
           #,@(build-partials name #'(a ...))
           [(a ... z) (#,fast a ... z)]
           [(a ... z . r) (apply (#,fast a ... z) r)])]))

  (meta define (func-wrap name args body)
    #`(let ([fast-path (lambda (#,@args) #,@body)])
        #,(func-cases name #'fast-path args)))

  ; function definition with slow/fast path. Slow path allows for
  ; under/overapplication. Fast path is exact application.
  ;
  ; The intent is for the scheme compiler to be able to recognize and
  ; optimize static, fast path calls itself, while still supporting
  ; unison-like automatic partial application and such.
  (define-syntax (define-unison x)
    (syntax-case x ()
      [(define-unison (name a ...) e ...)
       #`(define name
           #,(func-wrap #'name #'(a ...) #'(e ...)))]))

  ; call-by-name bindings
  (define-syntax name
    (syntax-rules ()
      ((name ([v (f . args)] ...) body)
       (let ([v (lambda r (apply f (append (list . args) r)))]
             ...)
         body))))

  ; wrapper that more closely matches `handle` constructs
  (define-syntax handle
    (syntax-rules ()
      [(handle [r ...] h e ...)
       (prompt p (fluid-let ([r (cons p h)] ...) e ...))]))

  ; wrapper that more closely matches ability requests
  (define-syntax request
    (syntax-rules ()
      [(request r t . args)
       ((cdr r) (list (quote r) t . args))]))

  (define-record-type
    data
    (fields type-ref payload))

  (define-syntax data-case
    (syntax-rules ()
      [(data-case scrut c ...)
       (record-case (data-payload scrut) c ...)]))

  (define (identity x) x)
  
  ; forces something that is expected to be a thunk, defined with
  ; e.g. `name` above. In some cases, we might have a normal value,
  ; so just do nothing in that case.
  (define (unison-force x)
    (if (procedure? x) (x) x))

  )
