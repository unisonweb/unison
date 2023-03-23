; This library implements missing bytevector functionality for unison
; builtins. The main missing bits are better support for immutable
; bytevectors. Both chez and racket have support for immutable
; bytevectors, but there is no standard API for dealing with them that
; implements all the functions we'd want. This library exports the
; desired functionality on top of an unsafe in-place freeze
; re-exported from the (unison core) module.
#!r6rs
(library (unison bytevector)
  (export
    freeze-bytevector!
    ibytevector-drop
    ibytevector-take
    u8-list->ibytevector)

  (import (rnrs)
          (unison core))

  (define (ibytevector-drop n bs)
    (let* ([l (bytevector-length bs)]
           [k (max 0 (- l n))]
           [br (make-bytevector k)])
      (bytevector-copy! bs n br 0 k)
      (freeze-bytevector! br)))

  (define (ibytevector-take n bs)
    (let* ([sz (min n (bytevector-length bs))]
           [br (make-bytevector sz)])
      (bytevector-copy! bs 0 br 0 sz)
      (freeze-bytevector! br)))

  (define (u8-list->ibytevector l)
    (freeze-bytevector! (u8-list->bytevector l))))
