#!r6rs

(library (unison concurrent)
  (export
    ref-new
    ref-read
    ref-write
    ref-cas
    promise-new
    promise-read
    promise-write
    promise-try-read
    fork
    kill
    sleep)

  (import (rnrs)
          (rnrs records syntactic)
          (rename
           (only (racket base)
                 box
                 unbox
                 set-box!
                 box-cas!
                 make-semaphore
                 semaphore-peek-evt
                 semaphore-post
                 sync/enable-break
                 thread
                 break-thread
                 parameterize-break
                 sleep
                 printf)
           (box ref-new)
           (unbox ref-read)
           (set-box! ref-write)
           (box-cas! ref-cas)
           (break-thread kill) ; TODO need to see whether the compiler wraps the exception for me
           (thread fork)
           (sleep sleep-secs))
          (only (racket unsafe ops) unsafe-struct*-cas!))

  (define none (cons 0 ()))
  (define (some a) (cons 1 a))
  (define (some? option) (eq? 1 (car option)))
  (define (get option) (cdr option))

  (define-record-type promise (fields semaphore event (mutable value)))

  (define (promise-new)
    (let* ([sem (make-semaphore)]
           [evt (semaphore-peek-evt sem)]
           [value none])
      (make-promise sem evt value)))

  (define (promise-try-read promise) (promise-value promise))

  (define (promise-read promise)
    (let loop ()
      (let* ([value (promise-value promise)])
        (cond
          [(some? value) (get value)]
          [else (sync/enable-break (promise-event promise)) (loop)]))))

  (define (promise-write promise new-value)
    (let loop ()
      (let* ([value (promise-value promise)]
             [cas! (lambda () (unsafe-struct*-cas! promise 2 value (some new-value)))]
             [awake-readers (lambda () (semaphore-post (promise-semaphore promise)))])
        (cond
          [(some? value) #f]
          [else
           (let ([ok (parameterize-break #f (if (cas!) (awake-readers) #f))])
             (if ok #t (loop)))]))))

  (define (sleep n) (sleep-secs (/ n 1000000))))
