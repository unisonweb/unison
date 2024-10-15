#lang racket/base
(require unison/data
         unison/chunked-seq
         unison/core
         unison/data-info
         racket/file
         racket/flonum
         (only-in racket
           date-dst?
           date-time-zone-offset
           date*-time-zone-name)
         (only-in unison/boot data-case define-unison-builtin)
         (only-in
           rnrs/arithmetic/flonums-6
           flmod))
(require racket/file)

(provide
 builtin-Clock.internals.systemTimeZone.v1
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

  builtin-IO.fileExists.impl.v3
  builtin-IO.fileExists.impl.v3:termlink
  builtin-IO.renameFile.impl.v3
  builtin-IO.renameFile.impl.v3:termlink
  builtin-IO.createDirectory.impl.v3
  builtin-IO.createDirectory.impl.v3:termlink
  builtin-IO.removeDirectory.impl.v3
  builtin-IO.removeDirectory.impl.v3:termlink
  builtin-IO.directoryContents.impl.v3
  builtin-IO.directoryContents.impl.v3:termlink
  builtin-IO.setCurrentDirectory.impl.v3
  builtin-IO.setCurrentDirectory.impl.v3:termlink
  builtin-IO.renameDirectory.impl.v3
  builtin-IO.renameDirectory.impl.v3:termlink
  builtin-IO.isDirectory.impl.v3
  builtin-IO.isDirectory.impl.v3:termlink
  builtin-IO.systemTime.impl.v3
  builtin-IO.systemTime.impl.v3:termlink
  builtin-IO.systemTimeMicroseconds.impl.v3
  builtin-IO.systemTimeMicroseconds.impl.v3:termlink
  builtin-IO.createTempDirectory.impl.v3
  builtin-IO.createTempDirectory.impl.v3:termlink)

(define (failure-result ty msg vl)
  (ref-either-left
    (ref-failure-failure
      ty
      (string->chunked-string msg)
      (unison-any-any vl))))

(define (getFileSize.impl.v3 path)
    (with-handlers
        [[exn:fail:filesystem?
           (lambda (e)
             (exception
               ref-iofailure:typelink
               (exception->string e)
               ref-unit-unit))]]
        (right (file-size (chunked-string->string path)))))

(define (getFileTimestamp.impl.v3 path)
    (with-handlers
        [[exn:fail:filesystem?
           (lambda (e)
             (exception
               ref-iofailure:typelink
               (exception->string e)
               ref-unit-unit))]]
        (right (file-or-directory-modify-seconds (chunked-string->string path)))))

; in haskell, it's not just file but also directory
(define-unison-builtin
  (builtin-IO.fileExists.impl.v3 path)
    (let ([path-string (chunked-string->string path)])
    (ref-either-right
        (or
        (file-exists? path-string)
        (directory-exists? path-string)))))

(define (removeFile.impl.v3 path)
    (delete-file (chunked-string->string path))
    (right none))

(define (getTempDirectory.impl.v3)
    (right (string->chunked-string (path->string (find-system-path 'temp-dir)))))

(define-unison-builtin
  (builtin-IO.setCurrentDirectory.impl.v3 path)
    (current-directory (chunked-string->string path))
    (ref-either-right none))

(define-unison-builtin
  (builtin-IO.directoryContents.impl.v3 path)
  (with-handlers
    [[exn:fail:filesystem?
       (lambda (e)
         (failure-result
           ref-iofailure:typelink
           (exception->string e)
           ref-unit-unit))]]
    (let* ([dirps (directory-list (chunked-string->string path))]
           [dirss (map path->string dirps)])
      (ref-either-right
        (vector->chunked-list
          (list->vector
            (map
              string->chunked-string
              (list* "." ".." dirss))))))))


(define-unison-builtin
  (builtin-IO.createTempDirectory.impl.v3 prefix)
    (ref-either-right
        (string->chunked-string
            (path->string
                (make-temporary-directory*
                    (string->bytes/utf-8
                        (chunked-string->string prefix)) #"")))))

(define-unison-builtin
  (builtin-IO.createDirectory.impl.v3 file)
    (make-directory (chunked-string->string file))
    (ref-either-right none))

(define-unison-builtin
  (builtin-IO.removeDirectory.impl.v3 file)
    (delete-directory/files (chunked-string->string file))
    (ref-either-right none))

(define-unison-builtin
  (builtin-IO.isDirectory.impl.v3 path)
    (ref-either-right
        (directory-exists? (chunked-string->string path))))

(define-unison-builtin
  (builtin-IO.renameDirectory.impl.v3 old new)
    (rename-file-or-directory (chunked-string->string old)
        (chunked-string->string new))
    (ref-either-right none))

(define-unison-builtin
  (builtin-IO.renameFile.impl.v3 old new)
    (rename-file-or-directory (chunked-string->string old)
        (chunked-string->string new))
    (ref-either-right none))

(define-unison-builtin
  (builtin-IO.systemTime.impl.v3 unit)
    (ref-either-right (current-seconds)))

(define-unison-builtin
  (builtin-IO.systemTimeMicroseconds.impl.v3 unit)
    (ref-either-right (inexact->exact (* 1000 (current-inexact-milliseconds)))))

(define-unison-builtin
  (builtin-Clock.internals.systemTimeZone.v1 secs)
  (let* ([d (seconds->date secs)])
    (list->unison-tuple
      (list
        (date-time-zone-offset d)
        (if (date-dst? d) 1 0)
        (date*-time-zone-name d)))))

(define (threadCPUTime.v1)
  (right
    (integer->time
      (current-process-milliseconds (current-thread)))))

(define (processCPUTime.v1)
  (right
    (integer->time
      (current-process-milliseconds #f))))

(define (realtime.v1)
  (right
    (float->time
      (current-inexact-milliseconds))))

(define (monotonic.v1)
  (right
    (float->time
      (current-inexact-monotonic-milliseconds))))

(define (integer->time msecs)
  (unison-timespec
    (truncate (/ msecs 1000))
    (* (modulo msecs 1000) 1000000)))

(define (float->time msecs)
  (unison-timespec
    (trunc (/ msecs 1000))
    (trunc (* (flmod msecs 1000.0) 1000000))))

;
(define (trunc f) (inexact->exact (truncate f)))

(define sec.v1 unison-timespec-sec)

(define nsec.v1 unison-timespec-nsec)
