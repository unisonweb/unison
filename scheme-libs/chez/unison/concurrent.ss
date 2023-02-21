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


  (define err "This operation is not supported on the pure Chez Scheme
backend, use the Racket over Chez Scheme backend")

  (define (ref-new a) (error err))
  (define (ref-read ref) (error err))
  (define (ref-write ref a) (error err))
  (define (ref-cas ref old-value new-value) (error err))
  (define (promise-new) (error err))
  (define (promise-read promise) (error err))
  (define (promise-try-read promise) (error err))
  (define (fork thread-thunk) (error err))
  (define (kill thread-id) (error err))
  (define (try-eval thunk) (error err)))
  
