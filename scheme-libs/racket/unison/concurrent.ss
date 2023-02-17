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
    sleep
    try-eval)

  (import (rnrs)
          (rnrs records syntactic)
          (unison data)
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
                 printf
                 exn:break?
                 with-handlers)
           (box ref-new)
           (unbox ref-read)
           (set-box! ref-write)
           (sleep sleep-secs))
          (only (racket unsafe ops) unsafe-struct*-cas!)
          (unison data))

  (define-record-type promise (fields semaphore event (mutable value)))

  (define (promise-new)
    (let* ([sem (make-semaphore)]
           [evt (semaphore-peek-evt sem)]
           [value none])
      (make-promise sem evt value)))

  (define (promise-try-read promise) (promise-value promise))

  (define (promise-read promise)
    (let loop ()
      (let ([value (promise-value promise)])
        (cond
          [(some? value) (option-get value)]
          [else (sync/enable-break (promise-event promise)) (loop)]))))

  (define (promise-write promise new-value)
    (let loop ()
      (let* ([value (promise-value promise)]
             [cas! (lambda () (unsafe-struct*-cas! promise 2 value (some new-value)))]
             [awake-readers (lambda () (semaphore-post (promise-semaphore promise)))])
        (cond
          [(some? value) false]
          [else
           (let ([ok (parameterize-break #f (if (cas!) (awake-readers) false))])
             (if ok true (loop)))]))))

  (define (ref-cas ref ticket value)
    (if (box-cas! ref ticket value) true false))

  (define (sleep n)
    (sleep-secs (/ n 1000000))
    (right unit))

  ;; Swallows uncaught breaks/thread kills rather than logging them to
  ;; match the behaviour of the Haskell runtime
  (define (fork thunk)
    (thread
     (lambda ()
       (with-handlers ([exn:break? (lambda (x) ())])
         (thunk)))))

  (define (kill threadId)
    (break-thread threadId)
    (right unit))

  ;; TODO Add proper type links to the various exception types once we have them
  (define (try-eval thunk)
    (with-handlers
      ([exn:break? (lambda (x) (exception "ThreadKilled" "thread killed" x))])
      (right (thunk)))))
