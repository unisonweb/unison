; This library implements missing bytevector functionality for unison
; builtins. The main missing bits are better support for immutable
; bytevectors. Chez does provide a way to make an immutable
; bytevector, but it copies its input, so implementing things that way
; would make many unncessary copies. This library instead implements
; functions on immutable bytevectors by directly freezing the newly
; created mutable vector. It also provides the freezing function,
; which is itself a unison builtin.
(library (unison bytevector)
  (export
    freeze-bv!
    ibytevector-drop
    ibytevector-take
    u8-list->ibytevector)

  (import (chezscheme))

  (define (freeze-bv! bs)
    (($primitive $bytevector-set-immutable!) bs)
    bs)

  (define (ibytevector-drop n bs)
    (let* ([l (bytevector-length bs)]
           [k (max 0 (- l n))]
           [br (make-bytevector k)])
      (bytevector-copy! bs n br 0 k)
      (freeze-bv! br)))

  (define (ibytevector-take n bs)
    (let* ([sz (min n (bytevector-length bs))]
           [br (make-bytevector sz)])
      (bytevector-copy! bs 0 br 0 sz)
      (freeze-bv! br)))

  (define (u8-list->ibytevector l) (freeze-bv! (u8-list->bytevector l))))
