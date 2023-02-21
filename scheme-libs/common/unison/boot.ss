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
#!r6rs
(library (unison boot)
  (export
    bytevector
    control
    define-unison
    handle
    identity
    name
    record-case
    request
    request-case
    unison-force)

  (import (rnrs)
          (unison core)
          (unison cont))

  ; Computes a symbol for automatically generated partial application
  ; cases, based on number of arguments applied. The partial
  ; application of `f` is (locally) named `f-partial-N`
  ; (meta define (partial-symbol name m)
  ;   (fun-sym (symbol->string name) "partial" (number->string m)))

  ; As above, but takes a syntactic object representing the arguments
  ; rather than their count.
  ; (define (partial-name name us)
  ;   (datum->syntax name (syntax->datum name)))

  (define-syntax with-name
    (syntax-rules ()
      [(with-name name e) (let ([name e]) name)]))

  ; function definition with slow/fast path. Slow path allows for
  ; under/overapplication. Fast path is exact application.
  ;
  ; The intent is for the scheme compiler to be able to recognize and
  ; optimize static, fast path calls itself, while still supporting
  ; unison-like automatic partial application and such.
  (define-syntax define-unison
    (lambda (x)
      (define (fast-path-symbol name)
        (string->symbol
          (string-append
            "fast-path-"
            (symbol->string name))))

      (define (fast-path-name name)
        (datum->syntax name (fast-path-symbol (syntax->datum name))))

      ; Helper function. Turns a list of syntax objects into a
      ; list-syntax object.
      (define (list->syntax l) #`(#,@l))
      ; Builds partial application cases for unison functions.
      ; It seems most efficient to have a case for each posible
      ; under-application.
      (define (build-partials name formals)
        (let rec ([us formals] [acc '()])
          (syntax-case us ()
            [() (list->syntax (cons #`[() #,name] acc))]
            [(a ... z)
             (rec #'(a ...)
                  (cons
                    #`[(a ... z)
                       (with-name
                         #,(datum->syntax name (syntax->datum name))
                         (lambda r (apply #,name a ... z r)))]
                    acc))])))

      ; Given an overall function name, a fast path name, and a list of
      ; arguments, builds the case-lambda body of a unison function that
      ; enables applying to arbitrary numbers of arguments.
      (define (func-cases name fast args)
        (syntax-case args ()
          [() #`(case-lambda
                  [() (#,fast)]
                  [r (apply (#,fast) r)])]
          [(a ... z)
           #`(case-lambda
               #,@(build-partials name #'(a ...))
               [(a ... z) (#,fast a ... z)]
               [(a ... z . r) (apply (#,fast a ... z) r)])]))

      (define (func-wrap name args body)
        (with-syntax ([fp (fast-path-name name)])
          #`(let ([fp (lambda (#,@args) #,@body)])
              #,(func-cases name #'fp args))))

      (syntax-case x ()
        [(define-unison (name a ...) e ...)
         #`(define name
             #,(func-wrap #'name #'(a ...) #'(e ...)))])))

  ; call-by-name bindings
  (define-syntax name
    (syntax-rules ()
      ((name ([v (f . args)] ...) body ...)
       (let ([v (lambda r (apply f (append (list . args) r)))]
             ...)
         body ...))))

  ; Wrapper that more closely matches `handle` constructs
  ;
  ; Note: this uses the prompt _twice_ to achieve the sort of dynamic
  ; scoping we want. First we push an outer delimiter, then install
  ; the continuation marks corresponding to the handled abilities
  ; (which tells which propt to use for that ability and which
  ; functions to use for each request). Then we re-delimit by the same
  ; prompt.
  ;
  ; If we just used one delimiter, we'd have a problem. If we pushed
  ; the marks _after_ the delimiter, then the continuation captured
  ; when handling would contain those marks, and would effectively
  ; retain the handler for requests within the continuation. If the
  ; marks were outside the prompt, we'd be in a similar situation,
  ; except where the handler would be automatically handling requests
  ; within its own implementation (although, in both these cases we'd
  ; get control errors, because we would be using the _function_ part
  ; of the handler without the necessary delimiters existing on the
  ; continuation). Both of these situations are wrong for _shallow_
  ; handlers.
  ;
  ; Instead, what we need to be able to do is capture the continuation
  ; _up to_ the marks, then _discard_ the marks, and this is what the
  ; multiple delimiters accomplish. There might be more efficient ways
  ; to accomplish this with some specialized mark functions, but I'm
  ; uncertain of what pitfalls there are with regard to that (whehter
  ; they work might depend on exact frame structure of the
  ; metacontinuation).
  (define-syntax handle
    (syntax-rules ()
      [(handle [r ...] h e ...)
       (let ([p (make-prompt)])
         (prompt0-at p
           (let ([v (let-marks (list (quote r) ...) (cons p h)
                      (prompt0-at p e ...))])
             (h (list 0 v)))))]))

  ; wrapper that more closely matches ability requests
  (define-syntax request
    (syntax-rules ()
      [(request r t . args)
       ((cdr (ref-mark (quote r))) (list (quote r) t . args))]))

  ; See the explanation of `handle` for a more thorough understanding
  ; of why this is doing two control operations.
  ;
  ; In-unison 'control' corresponds to a (shallow) handler jump, so we
  ; need to capture the continuation _and_ discard some dynamic scope
  ; information. The capture is accomplished via the first
  ; control0-at, while the second does the discard, based on the
  ; convention used in `handle`.
  (define-syntax control
    (syntax-rules ()
      [(control r k e ...)
       (let ([p (car (ref-mark r))])
         (control0-at p k (control0-at p _k e ...)))]))

  ; Wrapper around record-case that more closely matches request
  ; matching. This gets around having to manage an intermediate
  ; variable name during code emission that doesn't correspond to an
  ; actual ANF name, which was causing variable numbering problems in
  ; the code emitter. Hygienic macros are a much more convenient
  ; mechanism for this.
  (define-syntax request-case
    (syntax-rules (pure)
      [(request-case scrut
         [pure (pv ...) pe ...]
         [ability
           [effect (ev ...) ee ...]
           ...]
         ...)

       (record-case scrut
         [0 (pv ...) pe ...]
         [ability subscrut
           (record-case subscrut
             [effect (ev ...) ee ...]
             ...)]
         ...)]))

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
