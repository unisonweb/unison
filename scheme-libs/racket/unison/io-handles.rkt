#lang racket/base
(require racket/exn
         racket/string
         racket/file
         rnrs/io/ports-6
         (only-in racket empty?)
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
 (prefix-out
  builtin-IO.
  (combine-out
    seekHandle.impl.v3
    getLine.impl.v1
    getBuffering.impl.v3
    setBuffering.impl.v3
    ))

; Still to implement:
;    handlePosition.impl.v3
;    isSeekable.impl.v3
;    getChar.impl.v1
;    ready.impl.v1
;    isFileOpen.impl.v3
;    isFileEOF.impl.v3
;    setEcho.impl.v1
;    getEcho.impl.v1
;       - unsafe-port->file-descriptor

   )

; typeLink msg any
(define (Exception typeLink message payload)
    (let* ([x7 (unison-any-any payload)]
           [x8 (unison-failure-failure typeLink message x7)])
    (unison-either-left x8)))

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

(define-unison (getBuffering.impl.v3 handle)
    (case (file-stream-buffer-mode handle)
        [(none) (unison-either-right (unison-buffermode-no-buffering))]
        [(line) (unison-either-right
                  (unison-buffermode-line-buffering))]
        [(block) (unison-either-right
                   (unison-buffermode-block-buffering))]
        [(#f) (Exception 'IO "Unable to determine buffering mode of handle")]
        [else (Exception 'IO "Unexpected response from file-stream-buffer-mode")]))

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
            (Exception 'IO "Sized block buffering not supported"))))

