#lang racket/base
(require racket/exn
         racket/string
         racket/file
         (only-in racket empty?)
         compatibility/mlist
         unison/data
         unison/chunked-seq
         unison/core
         unison/tcp
         unison/pem
          unison/core
          unison/data
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
  unison-FOp-IO.
  (combine-out
   putBytes.impl.v3
   getBytes.impl.v3
   getLine.impl.v1
;    setEcho.impl.v1
;    setBuffering.impl.v3
;    getBuffering.impl.v3
;    handlePosition.impl.v3
;    seekHandle.impl.v3
;    isSeekable.impl.v3
;    getChar.impl.v1
;    ready.impl.v1
;    getEcho.impl.v1
;    isFileOpen.impl.v3
;    isFileEOF.impl.v3
;    closeFile.impl.v3
;    openFile.impl.v3
   )))

  (define (putBytes.impl.v3 p bs)
    (begin
      (write-bytes (chunked-bytes->bytes bs) p)
      (flush-output p)
      (sum 1 #f)))

  (define (getBytes.impl.v3 p n)
    (let* ([buffer (read-bytes n p)])
      (if (eof-object? buffer)
        (right (bytes->chunked-bytes (make-bytes 0)))
        (right (bytes->chunked-bytes buffer))
      )))

  (define (getLine.impl.v1 p)
    (let* ([line (read-line p)])
      (if (eof-object? line)
        (right (string->chunked-string ""))
        (right (string->chunked-string line))
      )))

