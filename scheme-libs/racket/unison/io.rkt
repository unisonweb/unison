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
         x509
         openssl)

(provide
 (prefix-out
  unison-FOp-IO.
  (combine-out
   getFileSize.impl.v3)))

(define (getFileSize.impl.v3 path)
    (with-handlers
        [[exn:fail:filesystem? (lambda (e) (exception "IOFailure" (exception->string e) '()))]]
        (file-size path)))

(define (getFileTimestamp.impl.v3 path)
    (with-handlers
        [[exn:fail:filesystem? (lambda (e) (exception "IOFailure" (exception->string e) '()))]]
        (file-or-directory-modify-seconds path)))
