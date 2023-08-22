#lang racket/base
(require racket/exn
         racket/string
         racket/file
         rnrs/io/ports-6
         (only-in rnrs standard-error-port standard-input-port standard-output-port vector-map)
         (only-in racket empty? with-output-to-string system/exit-code system false?)
         compatibility/mlist
         (only-in unison/boot data-case define-unison)
         unison/data
         unison/chunked-seq
         unison/core
         unison/tcp
         unison/pem
          unison/core
          unison/data
          unison/data-info
          unison/math
          unison/chunked-seq
          unison/chunked-bytes
          unison/bytes-nat
          unison/pattern
          unison/crypto
          unison/data
          unison/io
          unison/tls
          unison/tcp
          unison/gzip
          unison/zlib
          unison/concurrent
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
    (let* ([x7 (unison-any-any payload)]
           [x8 (unison-failure-failure typeLink message x7)])
    (unison-either-left x8)))

(define-unison (isFileOpen.impl.v3 port)
    (unison-either-right
        (if (port-closed? port) unison-boolean-false unison-boolean-true)))

(define-unison (ready.impl.v1 port)
    (if (byte-ready? port)
        (unison-either-right unison-boolean-true)
        (if (port-eof? port)
            (Exception 'IO "EOF" port)
            (unison-either-right unison-boolean-false))))

(define-unison (getCurrentDirectory.impl.v3 unit)
    (unison-either-right
      (string->chunked-string (path->string (current-directory)))))

(define-unison (seekHandle.impl.v3 handle mode amount)
    (data-case mode
        (0 ()
            (set-port-position! handle amount)
            (unison-either-right none))
        (1 ()
            (let ([current (port-position handle)])
                (set-port-position! handle (+ current amount))
                (unison-either-right none)))
        (2 ()
            (Exception 'BadNews "SeekFromEnd not supported" 0))))

(define-unison (getLine.impl.v1 handle)
  (let* ([line (read-line handle)])
    (if (eof-object? line)
        (unison-either-right (string->chunked-string ""))
        (unison-either-right (string->chunked-string line))
        )))

(define-unison (getChar.impl.v1 handle)
  (let* ([char (read-char handle)])
    (if (eof-object? char)
        (Exception 'isEOFError "End of file reached")
        (unison-either-right char))))

(define-unison (getSomeBytes.impl.v1 handle bytes)
  (let* ([buffer (make-bytes bytes)]
         [line (read-bytes-avail! buffer handle)])
    (if (eof-object? line)
        (unison-either-right (bytes->chunked-bytes #""))
        (unison-either-right (bytes->chunked-bytes buffer))
        )))

(define-unison (getBuffering.impl.v3 handle)
    (case (file-stream-buffer-mode handle)
        [(none) (unison-either-right (unison-buffermode-no-buffering))]
        [(line) (unison-either-right
                  (unison-buffermode-line-buffering))]
        [(block) (unison-either-right
                   (unison-buffermode-block-buffering))]
        [(#f) (Exception 'IO "Unable to determine buffering mode of handle" '())]
        [else (Exception 'IO "Unexpected response from file-stream-buffer-mode" '())]))

(define-unison (setBuffering.impl.v3 handle mode)
    (data-case mode
        (0 ()
            (file-stream-buffer-mode handle 'none)
            (unison-either-right none))
        (1 ()
            (file-stream-buffer-mode handle 'line)
            (unison-either-right none))
        (2 ()
            (file-stream-buffer-mode handle 'block)
            (unison-either-right none))
        (3 (size)
            (Exception 'IO "Sized block buffering not supported" '()))))

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
      (unison-either-right
        (if (get-stdin-echo) unison-boolean-true unison-boolean-false))
      (Exception 'IO "getEcho only supported on stdin" '())))

(define-unison (setEcho.impl.v1 handle echo)
  (if (eq? handle stdin)
      (begin
        (data-case echo
            (1 () (system "stty echo"))
            (0 () (system "stty -echo")))
        (unison-either-right none))
      (Exception 'IO "setEcho only supported on stdin" '())))

(define (get-stdin-echo)
  (let ([current (with-output-to-string (lambda () (system "stty -a")))])
    (string-contains? current " echo ")))

(define-unison (getArgs.impl.v1 unit)
    (unison-either-right
      (vector->chunked-list
        (vector-map string->chunked-string (current-command-line-arguments)))))

(define-unison (getEnv.impl.v1 key)
    (let ([value (environment-variables-ref (current-environment-variables) (string->bytes/utf-8 (chunked-string->string key)))])
        (if (false? value)
            (Exception 'IO "environmental variable not found" key)
            (unison-either-right
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
