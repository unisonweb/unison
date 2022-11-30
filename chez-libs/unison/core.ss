(library (unison core)
  (export
    identity

    describe-value
    decode-value)

  (import (chezscheme))

  (define (identity x) x)
  
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
  )
