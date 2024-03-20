#lang racket/base
(require unison/data
         unison/chunked-seq
         unison/core
         unison/data-info
         racket/file
         racket/flonum
         (only-in unison/boot data-case define-unison)
         (only-in
           rnrs/arithmetic/flonums-6
           flmod))
(require racket/file)

(provide
 (prefix-out
  unison-FOp-Clock.internals.
  (combine-out
    threadCPUTime.v1
    processCPUTime.v1
    realtime.v1
    monotonic.v1
    sec.v1
    nsec.v1))
 (prefix-out
  unison-FOp-IO.
  (combine-out
   getFileTimestamp.impl.v3
   getTempDirectory.impl.v3
   removeFile.impl.v3
   getFileSize.impl.v3))
  (prefix-out
    builtin-IO.
    (combine-out
        fileExists.impl.v3
        renameFile.impl.v3
        createDirectory.impl.v3
        removeDirectory.impl.v3
        setCurrentDirectory.impl.v3
        renameDirectory.impl.v3
        isDirectory.impl.v3
        systemTime.impl.v3
        systemTimeMicroseconds.impl.v3
        createTempDirectory.impl.v3)))

(define (getFileSize.impl.v3 path)
    (with-handlers
        [[exn:fail:filesystem?
           (lambda (e)
             (exception unison-iofailure:typelink (exception->string e) '()))]]
        (right (file-size (chunked-string->string path)))))

(define (getFileTimestamp.impl.v3 path)
    (with-handlers
        [[exn:fail:filesystem?
           (lambda (e)
             (exception unison-iofailure:typelink (exception->string e) '()))]]
        (right (file-or-directory-modify-seconds (chunked-string->string path)))))

; in haskell, it's not just file but also directory
(define-unison (fileExists.impl.v3 path)
    (let ([path-string (chunked-string->string path)])
    (unison-either-right
        (or
        (file-exists? path-string)
        (directory-exists? path-string)))))

(define (removeFile.impl.v3 path)
    (delete-file (chunked-string->string path))
    (right none))

(define (getTempDirectory.impl.v3)
    (right (string->chunked-string (path->string (find-system-path 'temp-dir)))))

(define-unison (setCurrentDirectory.impl.v3 path)
    (current-directory (chunked-string->string path))
    (unison-either-right none))

(define-unison (createTempDirectory.impl.v3 prefix)
    (unison-either-right
        (string->chunked-string
            (path->string
                (make-temporary-directory*
                    (string->bytes/utf-8
                        (chunked-string->string prefix)) #"")))))

(define-unison (createDirectory.impl.v3 file)
    (make-directory (chunked-string->string file))
    (unison-either-right none))

(define-unison (removeDirectory.impl.v3 file)
    (delete-directory/files (chunked-string->string file))
    (unison-either-right none))

(define-unison (isDirectory.impl.v3 path)
    (unison-either-right
        (directory-exists? (chunked-string->string path))))

(define-unison (renameDirectory.impl.v3 old new)
    (rename-file-or-directory (chunked-string->string old)
        (chunked-string->string new))
    (unison-either-right none))

(define-unison (renameFile.impl.v3 old new)
    (rename-file-or-directory (chunked-string->string old)
        (chunked-string->string new))
    (unison-either-right none))

(define-unison (systemTime.impl.v3 unit)
    (unison-either-right (current-seconds)))

(define-unison (systemTimeMicroseconds.impl.v3 unit)
    (unison-either-right (inexact->exact (* 1000 (current-inexact-milliseconds)))))

(define (threadCPUTime.v1)
    (right (current-process-milliseconds (current-thread))))
(define (processCPUTime.v1)
    (right (current-process-milliseconds 'process)))
(define (realtime.v1)
    (right (current-inexact-milliseconds)))
(define (monotonic.v1)
    (right (current-inexact-monotonic-milliseconds)))

;
(define (flt f) (fl->exact-integer (fltruncate f)))

(define (sec.v1 ts) (flt (/ ts 1000)))

(define (nsec.v1 ts) (flt (* (flmod ts 1000.0) 1000000)))
