#lang racket/base

(require unison/boot
         unison/data
         unison/data-info)

(provide
  builtin-ImmutableArray.copyTo!
  builtin-ImmutableArray.copyTo!:termlink
  builtin-ImmutableArray.read
  builtin-ImmutableArray.read:termlink
  builtin-ImmutableArray.size
  builtin-ImmutableArray.size:termlink
  builtin-ImmutableByteArray.copyTo!
  builtin-ImmutableByteArray.copyTo!:termlink
  builtin-ImmutableByteArray.read16be
  builtin-ImmutableByteArray.read16be:termlink
  builtin-ImmutableByteArray.read24be
  builtin-ImmutableByteArray.read24be:termlink
  builtin-ImmutableByteArray.read32be
  builtin-ImmutableByteArray.read32be:termlink
  builtin-ImmutableByteArray.read40be
  builtin-ImmutableByteArray.read40be:termlink
  builtin-ImmutableByteArray.read64be
  builtin-ImmutableByteArray.read64be:termlink
  builtin-ImmutableByteArray.read8
  builtin-ImmutableByteArray.read8:termlink
  builtin-ImmutableByteArray.size
  builtin-ImmutableByteArray.size:termlink

  builtin-MutableArray.copyTo!
  builtin-MutableArray.copyTo!:termlink
  builtin-MutableArray.freeze
  builtin-MutableArray.freeze:termlink
  builtin-MutableArray.freeze!
  builtin-MutableArray.freeze!:termlink
  builtin-MutableArray.read
  builtin-MutableArray.read:termlink
  builtin-MutableArray.size
  builtin-MutableArray.size:termlink
  builtin-MutableArray.write
  builtin-MutableArray.write:termlink
  builtin-MutableByteArray.copyTo!
  builtin-MutableByteArray.copyTo!:termlink
  builtin-MutableByteArray.freeze!
  builtin-MutableByteArray.freeze!:termlink
  builtin-MutableByteArray.read16be
  builtin-MutableByteArray.read16be:termlink
  builtin-MutableByteArray.read24be
  builtin-MutableByteArray.read24be:termlink
  builtin-MutableByteArray.read32be
  builtin-MutableByteArray.read32be:termlink
  builtin-MutableByteArray.read40be
  builtin-MutableByteArray.read40be:termlink
  builtin-MutableByteArray.read64be
  builtin-MutableByteArray.read64be:termlink
  builtin-MutableByteArray.read8
  builtin-MutableByteArray.read8:termlink
  builtin-MutableByteArray.size
  builtin-MutableByteArray.size:termlink
  builtin-MutableByteArray.write16be
  builtin-MutableByteArray.write16be:termlink
  builtin-MutableByteArray.write32be
  builtin-MutableByteArray.write32be:termlink
  builtin-MutableByteArray.write64be
  builtin-MutableByteArray.write64be:termlink
  builtin-MutableByteArray.write8
  builtin-MutableByteArray.write8:termlink)

(define (handle-with-ability thunk)
  (with-handlers
    ([exn:fail:contract?
       (lambda (e)
         (request
           ref-exception
           0
           (ref-failure-failure
             ref-arrayfailure:typelink
             (string->chunked-string (exception->string e))
             (unison-any-any ref-unit-unit))))])
    (thunk)))

(define-syntax handle-array
  (syntax-rules ()
    [(_ . es) (handle-with-ability (lambda () . es))]))

(define-unsion-builtin
  (builtin-ImmutableArray.copyTo! dst doff src soff n)
  (handle-array
    (vector-copy! dst doff src soff n)
    ref-unit-unit))

(define-unsion-builtin (builtin-ImmutableArray.read arr i)
  (handle-array (vector-ref arr i)))

(define-unsion-builtin (builtin-ImmutableArray.size arr)
  (vector-length arr))

(define-unsion-builtin
  (builtin-ImmutableByteArray.copyTo! dst doff src soff n)
  (handle-array
    (bytes-copy! dst doff src soff n)
    ref-unit-unit))

(define-unsion-builtin (builtin-ImmutableByteArray.read16be arr i)
  (handle-array (bytevector-u16-ref arr i 'big)))

(define-unsion-builtin (builtin-ImmutableByteArray.read24be arr i)
  (handle-array (bytevector-u24-ref arr i 'big)))

(define-unsion-builtin (builtin-ImmutableByteArray.read32be arr i)
  (handle-array (bytevector-u32-ref arr i 'big)))

(define-unsion-builtin (builtin-ImmutableByteArray.read40be arr i)
  (handle-array (bytevector-u40-ref arr i 'big)))

(define-unsion-builtin (builtin-ImmutableByteArray.read64be arr i)
  (handle-array (bytevector-u64-ref arr i 'big)))

(define-unsion-builtin (builtin-ImmutableByteArray.read8 arr i)
  (handle-array (bytevector-u8-ref arr i)))

(define-unsion-builtin (builtin-ImmutableByteArray.size arr)
  (bytevector-length arr))

(define-unsion-builtin (builtin-MutableArray.copyTo! dst doff src soff l)
  (handle-array
    (vector-copy! dst doff src soff l)
    ref-unit-unit))

(define-unsion-builtin (builtin-MutableArray.freeze arr i j)
  (handle-array
    (freeze-subvector arr i j)))

(define-unsion-builtin (builtin-MutableArray.freeze! arr)
  (freeze-vector! arr))

(define-unsion-builtin (builtin-MutableArray.read arr i)
  (handle-array (vector-ref arr i)))

(define-unsion-builtin (builtin-MutableArray.size arr)
  (vector-length arr))

(define-unsion-builtin (builtin-MutableArray.write dst i x)
  (handle-array
    (vector-set! dst i x)
    ref-unit-unit))

(define-unsion-builtin
  (builtin-MutableByteArray.copyTo! dst doff src soff l)
  (handle-array
    (bytes-copy! dst doff src soff l)
    ref-unit-unit))

(define-unsion-builtin (builtin-MutableByteArray.freeze! arr)
  (freeze-bytevector! arr))

(define-unsion-builtin (builtin-MutableByteArray.read16be arr i)
  (handle-array (bytevector-u16-ref arr i 'big)))

(define-unsion-builtin (builtin-MutableByteArray.read24be arr i)
  (handle-array (bytevector-u24-ref arr i 'big)))

(define-unsion-builtin (builtin-MutableByteArray.read32be arr i)
  (handle-array (bytevector-u32-ref arr i 'big)))

(define-unsion-builtin (builtin-MutableByteArray.read40be arr i)
  (handle-array (bytevector-u40-ref arr i 'big)))

(define-unsion-builtin (builtin-MutableByteArray.read64be arr i)
  (handle-array (bytevector-u64-ref arr i 'big)))

(define-unsion-builtin (builtin-MutableByteArray.read8 arr i)
  (handle-array (bytevector-u8-ref arr i)))

(define-unsion-builtin (builtin-MutableByteArray.size arr)
  (bytevector-length arr))

(define-unsion-builtin (builtin-MutableByteArray.write16be arr i m)
  (handle-array
    (bytevector-u16-set! arr i m 'big)
    ref-unit-unit))

(define-unsion-builtin (builtin-MutableByteArray.write32be arr i m)
  (handle-array
    (bytevector-u32-set! arr i m 'big)
    ref-unit-unit))

(define-unsion-builtin (builtin-MutableByteArray.write64be arr i m)
  (handle-array
    (bytevector-u64-set! arr i m 'big)
    ref-unit-unit))

(define-unsion-builtin (builtin-MutableByteArray.write8 arr i m)
  (handle-array
    (bytevector-u8-set! arr i m)
    ref-unit-unit))
