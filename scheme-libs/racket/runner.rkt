#lang racket/base

(require
  racket/pretty
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
     (require unison/boot)
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

(define (write-module srcf main-ref icode)
  (call-with-output-file
    srcf
    (lambda (port)
      (parameterize ([print-as-expression #t])
        (display "#lang racket/base\n\n" port)

        (for ([expr (build-intermediate-expressions main-ref icode)])
          (pretty-print expr port 1)
          (newline port))
        (newline port)))
    #:exists 'replace))

(define (do-compile output)
  (define-values (icode main-ref) (decode-input))

  (define srcf (path->string (path-replace-extension output ".rkt")))
  (define dstf (embedding-executable-add-suffix output #f))
  (define chop (path->string (path-replace-extension output "")))
  (define mod-sym (string->symbol (string-append "#%unison:" chop)))

  (define primary
    (parameterize ([current-namespace (make-base-namespace)])
      (compile
        `(begin
           (namespace-require '',mod-sym)
           (when (module-declared? '',mod-sym)
             ((dynamic-require '',mod-sym 'main)))))))

  (write-module srcf main-ref icode)

  (create-embedding-executable
    dstf
    #:modules `((#%unison: (file ,srcf)))
    #:variant 'cs
    ; #:cmdline '("-U" "--")
    #:configure-via-first-module? #t
    #:literal-expressions (list primary)))

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
