; This library is intended to contain the implementation of
; delimited continuations used in the semantics of abilities.
;
; Currently, it is a somewhat naive implementation based on call/cc.
; This has known issues that seem to still be in force even though a
; tweak has been applied that should fix certain space leaks. So, in
; the future it will likely need to be rewritten using some
; implementation specific machinery (possibly making use of
; continuation attachments as in the Racket implementation).
;
; Also, although the API includes prompts, they are currently ignored
; in `control` and `prompt` always uses the same prompt (0). This
; means that nesting handlers will not work in general, since requests
; will not be able to jump over an inner handler of unrelated
; abilities. It should be sufficient for testing simple examples in
; the mean time, though.
(library (unison cont)
  (export prompt control)

  (import (chezscheme))

  (define mk (lambda (x) (raise "fell off end")))

  (define (prompt-impl h)
    ((call/cc
       (lambda (k)
         (let ([ok mk])
           (set! mk (lambda (x) (set! mk ok) (k x)))
           ; (h 0) <-- prompt = 0
           (mk (let ([v (h 0)]) (lambda () v))))))))

  (define-syntax prompt
    (syntax-rules ()
      [(prompt p e ...)
       (prompt-impl (lambda (p) e ...))]))

  (define (control-impl h)
    (call/cc
      (lambda (k)
        (let* ([g (lambda () (prompt p (h k)))])
          (mk g)))))

  (define-syntax control
    (syntax-rules ()
      [(control p k e ...)
       (control-impl (lambda (k) e ...))])))

