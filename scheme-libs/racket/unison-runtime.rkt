#lang racket/base

; This implements a standalone unison runtime, with options for
; generating compilable racket modules.
;
; For runtime, it relies on the support for unison dynamic code
; loading. It expects to be provided with a serialized list of term
; links and associated code. It then loads the code in the same manner
; as dynamic runtime execution, and evaluates a main definition.
;
; Since this is intended to be an implementation of evaluation for
; e.g. ucm, the input is expected to be complete. No protocol is
; implemented for negotiating with a host for additional needed
; definitions. The program has all the built in definitions, and
; everything else is expected to be provided in the initial input.
;
; In addition to this mode, it is possible to supply a command line
; argument `-G` with a file name. This will instead produce a racket
; file with the supplied definitions. This file should be suitable for
; compilation and distribution with the `raco` tool, so long as the
; supporting unison-on-racket libraries are known to the racket
; install.

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

; Gets bytes using the expected input format. The format is simple:
;
;  - 4 bytes indicating how many bytes follow
;  - the actual payload, with size matching the above
(define (grab-bytes port)
  (let* ([size-bytes (read-bytes 4 port)]
         [size (integer-bytes->integer size-bytes #f #t 0 4)])
    (read-bytes size port)))

; Reads and decodes the input. First uses `grab-bytes` to read the
; payload, then uses unison functions to deserialize the `Value` that
; is expected.
;
; The `Value` is expected to be a pair of loadable code and which
; definition should be executed. In unison types, it is:
;
;   ([(Link.Term, Code)], Link.Term)
(define (decode-input port)
  (let ([bs (grab-bytes port)])
    (match (builtin-Value.deserialize (bytes->chunked-bytes bs))
      [(unison-data _ t (list q))
       (= t ref-either-right:tag)
       (apply
         values
         (unison-tuple->list (reify-value (unison-quote-val q))))]
      [else
        (raise "unexpected input")])))

(define (encode-output tag result port)
  (let* ([val (unison-quote (reflect-value result))]
         [bs (chunked-bytes->bytes (builtin-Value.serialize val))])
    (write-bytes tag port)
    (write-bytes bs port)
    (void)))

(define (encode-success result port)
  (encode-output #"0000" result port))

(define (encode-error fail port)
  (match fail
    [(exn:bug msg val)
     (encode-output #"0001" fail port)]))

(define ((eval-exn-handler port) rq)
  (request-case rq
    [pure (result) (encode-success result port)]
    [ref-exception:typelink
      [0 (fail)
        (control ref-exception:typelink k
          (encode-error fail port))]]))

; Implements the evaluation mode of operation. First decodes the
; input. Then uses the dynamic loading machinery to add the code to
; the runtime. Finally executes a specified main reference.
(define (do-evaluate in out)
  (let-values ([(code main-ref) (decode-input in)])
    (add-runtime-code 'unison-main code)
    (with-handlers
      ([exn:bug? (lambda (e) (encode-error e out))])

      (handle [ref-exception:typelink] (eval-exn-handler out)
              ((termlink->proc main-ref))))))

; Uses racket pretty printing machinery to instead generate a file
; containing the given code, and which executes the main definition on
; loading. This file can then be built with `raco exe`.
(define (write-module srcf main-ref icode)
  (call-with-output-file
    srcf
    (lambda (port)
      (parameterize ([print-as-expression #t])
        (display "#lang racket/base\n\n" port)

        (for ([expr (build-intermediate-module main-ref icode)])
          (pretty-print expr port 1)
          (newline port))
        (newline port)))
    #:exists 'replace))

; Decodes input and writes a module to the specified file.
(define (do-generate srcf)
  (define-values (icode main-ref) (decode-input (current-input-port)))
  (write-module srcf main-ref icode))

(define generate-to (make-parameter #f))
(define show-version (make-parameter #f))
(define use-port-num (make-parameter #f))

(define (handle-command-line)
  (command-line
    #:program "unison-runtime"
    #:once-any
    ["--version"
     "display version"
     (show-version #t)]
    [("-p" "--port")
     port-num
     "runtime communication port"
     (use-port-num port-num)]
    [("-G" "--generate-file")
       file
       "generate code to <file>"
       (generate-to file)]))

(begin
  (handle-command-line)
  (cond
    [(show-version) (displayln "unison-runtime version 0.0.11")]
    [(generate-to) (do-generate (generate-to))]
    [(use-port-num)
     (let-values ([(in out) (tcp-connect "localhost" (use-port-num))])
       (do-evaluate in out)
       (close-output-port out)
       (close-input-port in))]
    [else
      (do-evaluate (current-input-port) (open-output-bytes))]))
