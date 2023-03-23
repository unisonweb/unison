
#!r6rs
(library (unison vector)
  (export
    freeze-vector!
    freeze-subvector)

  (import (rnrs)
          (unison core)
          (unison data))

  (define (freeze-subvector src off len)
    (let ([dst (make-vector len)])
      (let next ([i (fx1- len)])
        (if (< i 0)
          (begin
            (freeze-vector! dst)
            (sum 1 dst))
          (begin
            (vector-set! dst i (vector-ref src (+ off i)))
            (next (fx1- i)))))))
  )
