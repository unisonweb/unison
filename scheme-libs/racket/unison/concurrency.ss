#!r6rs

(library (unison concurrency)
  (export
    promise
    make-promise
    promise-read
    promise-write
    promise-try-read
    fork)

  (import (rnrs)
          (rnrs records syntactic)
          (rename
           (only (racket base)
                 make-semaphore
                 semaphore-peek-evt
                 sync/enable-break
                 semaphore-post
                 parameterize-break
                 thread
                 printf
                 sleep)
           (break-thread kill) ; TODO need to see whether the compiler wraps the exception for me
           (thread fork))
          (only (racket unsafe ops) unsafe-struct*-cas!))

  (define none (cons 0 ()))
  (define (some a) (cons 1 a))
  (define (some? option) (eq? 1 (car option)))
  (define (get option) (cdr option))

  (define-record-type
    (promise new-promise promise?)
    (fields semaphore event (mutable value)))

  (define (make-promise)
    (let* ([sem (make-semaphore)]
           [evt (semaphore-peek-evt sem)]
           [value none])
      (new-promise sem evt value)))

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

  ;;; tests

  (define (spawn-promise-reader name p)
    (fork
     (lambda ()
       (printf "Thread ~a started ~n" name)
       (printf "Thread ~a finished with result ~a ~n" name (promise-read p)))))

  (define (test-promise)
    (let ([p (make-promise)])
      (printf "Main thread started ~n")
      (printf "Current promise is ~a ~n" (promise-try-read p))
      (spawn-promise-reader "foo" p)
      (spawn-promise-reader "bar" p)
      (sleep 3)
      (promise-write p 42)
      (sleep 1)
      (promise-write p 73)
      (spawn-promise-reader "baz" p)
      (printf "Current promise is ~a ~n" (promise-try-read p))
      (printf "Main thread finished ~n"))))
