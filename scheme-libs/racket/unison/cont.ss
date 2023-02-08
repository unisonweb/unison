
#!r6rs
(library (unison cont)
  (export
    prompt
    control)

  (import
    (rnrs)
    (unison core)
    (only (racket)
          make-continuation-prompt-tag)
    (only (racket control)
          prompt0-at
          control0-at))

  (define-syntax prompt
    (syntax-rules ()
      [(prompt p e ...)
       (let ([p (make-continuation-prompt-tag)])
         (prompt0-at p
           e ...))]))

  (define-syntax control
    (syntax-rules ()
      [(control r k e ...)
       (control0-at (car (ref-mark r)) k e ...)])))

