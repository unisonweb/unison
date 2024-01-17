#!racket/base

(require
  (except-in racket false true unit any)
  compiler/embed
  unison/boot
  unison/data
  unison/data-info
  unison/chunked-seq
  unison/primops
  unison/primops-generated
  unison/builtin-generated)

(define (grab-bytes)
  (let* ([size-bytes (read-bytes 4)]
         [size (integer-bytes->integer size-bytes #f #t 0 4)])
    (read-bytes size)))

(define (decode-input)
  (let ([bs (grab-bytes)])
    (match (builtin-Value.deserialize (bytes->chunked-bytes bs))
      [(unison-data _ t (list q))
       (= t unison-either-right:tag)
       (apply
         values
         (unison-tuple->list (reify-value (unison-quote-val q))))]
      [else
        (raise "unexpected input")])))

(define (build-main-module main-def)
  `(module unison-main racket/base
     (require
       unison/boot)

     (provide main)

     (define (main)
       (handle ['ref-4n0fgs00] top-exn-handler
               (,(termlink->name main-def))))))

(define (do-evaluate)
  (let-values ([(code main-ref) (decode-input)])
    (add-runtime-code 'unison-main code)
    (handle ['ref-4n0fgs00] top-exn-handler
            ((termlink->proc main-ref))
            (data 'unit 0))))

; stub implementation
(define (do-compile output) (void))
  ; (let-values ([(code main-ref) (decode-input)])
  ;   (create-embedding-executable
  ;     output
  ;     #:modules '((#f unison-main))
  ;     #:literal-expression '(begin (require unison-main) (main)))))

(define runtime-namespace
  (let ([ns (variable-reference->namespace (#%variable-reference))])
    (namespace-require ''#%kernel ns)
    ns))

(define (chunked-list->list cl)
  (vector->list (chunked-list->vector cl)))

(define (list->chunked-list l)
  (vector->chunked-list (list->vector l)))

(define (join ls)
  (cond
    [(null? ls) '()]
    [else (append (car ls) (join (cdr ls)))]))

(define compile (make-parameter #f))

(define (handle-command-line)
  (command-line
    #:program "runner"
    #:once-any
    [("-o" "--output")
       file
       "compile to <file>"
       (compile file)]
    #:args ()
    (compile)))

(let ([out (handle-command-line)])
  (if out
    (do-compile out)
    (do-evaluate)))
