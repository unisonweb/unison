
#lang racket/base

(require unison/boot
         unison/data
         unison/data-info)

(provide
  builtin-Boolean.not
  builtin-Boolean.not:termlink

  builtin-Any.Any
  builtin-Any.Any:termlink
  builtin-Any.unsafeExtract
  builtin-Any.unsafeExtract:termlink

  builtin-Debug.toText
  builtin-Debug.toText:termlink
  builtin-Debug.trace
  builtin-Debug.trace:termlink
  builtin-Debug.watch
  builtin-Debug.watch:termlink

  builtin-Scope.run
  builtin-Scope.run:termlink

  builtin-bug
  builtin-bug:termlink

  builtin-jumpCont
  builtin-jumpCont:termlink
  builtin-todo
  builtin-todo:termlink)



(define-unison-builtin (builtin-Boolean.not b) (not b))

(define-unison-builtin (builtin-Any.Any x) (ref-any-any x))

(define-unison-builtin (builtin-Any.unsafeExtract x)
  (match x
    [(unison-data r t (list x)) x]))

(define-unison-builtin (builtin-Debug.toText v)
  (ref-optional-some
    (ref-either-left
      (string->chunked-string
        (describe-value v)))))

(define-unison-builtin (builtin-Debug.trace msg v)
  (display "trace: ")
  (displayln (chunked-string->string msg))
  (displayln (describe-value v))
  ref-unit-unit)

(define-unison-builtin (builtin-Debug.watch msg v)
  (displayln (chunked-string->string msg))
  v)

(define-unison-builtin (builtin-bug x)
  (raise (make-exn:bug "builtin.bug" x)))

(define-unison-builtin (builtin-jumpCont k) k)

(define-unison-builtin (builtin-todo x)
  (raise (make-exn:bug "builtin.todo" x)))

(define-unison-builtin (builtin-Scope.run k)
  (k ref-unit-unit))
