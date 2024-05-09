#lang racket/base
(require racket/string
         rnrs/io/ports-6
         (only-in rnrs standard-error-port standard-input-port standard-output-port vector-map)
         (only-in racket empty? with-output-to-string system/exit-code system false?)
         (only-in unison/boot data-case define-unison)
         unison/data
         unison/chunked-seq
         unison/data
         unison/data-info
         unison/chunked-seq
         unison/data
         )

(provide
 unison-FOp-IO.stdHandle
 (prefix-out
  builtin-IO.
  (combine-out
    seekHandle.impl.v3
    getLine.impl.v1
    getSomeBytes.impl.v1
    getBuffering.impl.v3
    setBuffering.impl.v3
    getEcho.impl.v1
    setEcho.impl.v1
    getArgs.impl.v1
    getEnv.impl.v1
    getChar.impl.v1
    isFileOpen.impl.v3
    isSeekable.impl.v3
    handlePosition.impl.v3
    process.call
    getCurrentDirectory.impl.v3
    ready.impl.v1
    ))

; Still to implement:
;    handlePosition.impl.v3
;    isSeekable.impl.v3
;    getChar.impl.v1
   )

; typeLink msg any
(define (Exception typeLink message payload)
    (let* ([a (unison-any-any payload)]
           [msg (string->chunked-string message)]
           [f (ref-failure-failure typeLink msg a)])
    (ref-either-left f)))

(define-unison (isFileOpen.impl.v3 port)
    (ref-either-right (not (port-closed? port))))

(define-unison (ready.impl.v1 port)
    (if (byte-ready? port)
        (ref-either-right #t)
        (if (port-eof? port)
            (Exception ref-iofailure:typelink "EOF" port)
            (ref-either-right #f))))

(define-unison (getCurrentDirectory.impl.v3 unit)
    (ref-either-right
      (string->chunked-string (path->string (current-directory)))))

(define-unison (isSeekable.impl.v3 handle)
    (ref-either-right
        (port-has-set-port-position!? handle)))

(define-unison (handlePosition.impl.v3 handle)
    (ref-either-right (port-position handle)))

(define-unison (seekHandle.impl.v3 handle mode amount)
    (data-case mode
        (0 ()
            (set-port-position! handle amount)
            (ref-either-right none))
        (1 ()
            (let ([current (port-position handle)])
                (set-port-position! handle (+ current amount))
                (ref-either-right none)))
        (2 ()
            (Exception
              ref-iofailure:typelink
              "SeekFromEnd not supported"
              0))))

(define-unison (getLine.impl.v1 handle)
  (let* ([line (read-line handle)])
    (if (eof-object? line)
        (ref-either-right (string->chunked-string ""))
        (ref-either-right (string->chunked-string line))
        )))

(define-unison (getChar.impl.v1 handle)
  (let* ([char (read-char handle)])
    (if (eof-object? char)
        (Exception
          ref-iofailure:typelink
          "End of file reached"
          ref-unit-unit)
        (ref-either-right char))))

(define-unison (getSomeBytes.impl.v1 handle nbytes)
  (let* ([buffer (make-bytes nbytes)]
         [line (read-bytes-avail! buffer handle)])
    (cond
      [(eof-object? line)
       (ref-either-right (bytes->chunked-bytes #""))]
      [(procedure? line)
       (Exception
         ref-iofailure:typelink
         "getSomeBytes.impl: special value returned"
         ref-unit-unit)]
      [else
       (ref-either-right
         (bytes->chunked-bytes
           (if (< line nbytes)
             (subbytes buffer 0 line)
             buffer)))])))

(define-unison (getBuffering.impl.v3 handle)
    (case (file-stream-buffer-mode handle)
        [(none) (ref-either-right ref-buffermode-no-buffering)]
        [(line) (ref-either-right
                  ref-buffermode-line-buffering)]
        [(block) (ref-either-right
                   ref-buffermode-block-buffering)]
        [(#f) (Exception
                ref-iofailure:typelink
                "Unable to determine buffering mode of handle"
                ref-unit-unit)]
        [else (Exception
                ref-iofailure:typelink
                "Unexpected response from file-stream-buffer-mode"
                ref-unit-unit)]))

(define-unison (setBuffering.impl.v3 handle mode)
    (data-case mode
        (0 ()
            (file-stream-buffer-mode handle 'none)
            (ref-either-right none))
        (1 ()
            (file-stream-buffer-mode handle 'line)
            (ref-either-right none))
        (2 ()
            (file-stream-buffer-mode handle 'block)
            (ref-either-right none))
        (3 (size)
            (Exception
              ref-iofailure:typelink
              "Sized block buffering not supported"
              ref-unit-unit))))

(define (with-buffer-mode port mode)
  (file-stream-buffer-mode port mode)
  port)

(define stdin (with-buffer-mode (standard-input-port) 'none))
(define stdout (with-buffer-mode (standard-output-port) 'line))
(define stderr (with-buffer-mode (standard-error-port) 'line))

(define (unison-FOp-IO.stdHandle n)
  (case n
    [(0) stdin]
    [(1) stdout]
    [(2) stderr]))

(define-unison (getEcho.impl.v1 handle)
  (if (eq? handle stdin)
      (ref-either-right (get-stdin-echo))
      (Exception
        ref-iofailure:typelink
        "getEcho only supported on stdin"
        ref-unit-unit)))

(define-unison (setEcho.impl.v1 handle echo)
  (if (eq? handle stdin)
      (begin
        (if echo
            (system "stty echo")
            (system "stty -echo"))
        (ref-either-right none))
      (Exception
        ref-iofailure:typelink
        "setEcho only supported on stdin"
        ref-unit-unit)))

(define (get-stdin-echo)
  (let ([current (with-output-to-string (lambda () (system "stty -a")))])
    (string-contains? current " echo ")))

(define-unison (getArgs.impl.v1 unit)
    (ref-either-right
      (vector->chunked-list
        (vector-map string->chunked-string (current-command-line-arguments)))))

(define-unison (getEnv.impl.v1 key)
    (let ([value (environment-variables-ref (current-environment-variables) (string->bytes/utf-8 (chunked-string->string key)))])
        (if (false? value)
            (Exception
              ref-iofailure:typelink
              "environmental variable not found"
              key)
            (ref-either-right
              (string->chunked-string (bytes->string/utf-8 value))))))

;; From https://github.com/sorawee/shlex/blob/5de06500e8c831cfc8dffb99d57a76decc02c569/main.rkt (MIT License)
;; with is a port of https://github.com/python/cpython/blob/bf2f76ec0976c09de79c8827764f30e3b6fba776/Lib/shlex.py#L325
(define unsafe-pattern #rx"[^a-zA-Z0-9_@%+=:,./-]")
(define (quote-arg s)
  (if (non-empty-string? s)
      (if (regexp-match unsafe-pattern s)
          (string-append "'" (string-replace s "'" "'\"'\"'") "'")
          s)
      "''"))

(define-unison (process.call command arguments)
  (system/exit-code
          (string-join (cons
                        (chunked-string->string command)
                        (map (lambda (arg) (quote-arg (chunked-string->string arg)))
                        (vector->list
                             (chunked-list->vector arguments))))
                             " ")))
