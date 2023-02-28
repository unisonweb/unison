; This library implements pimitive operations that are used in
; builtins. There are two different sorts of primitive operations, but
; the difference is essentially irrelevant except for naming schemes.
;
; POps are part of a large enumeration of 'instructions' directly
; implemented in the Haskell runtime. These are referred to using the
; naming scheme `unison-POp-INST` where `INST` is the name of the
; instruction, which is (at the time of this writing) 4 letters.
;
; FOps are 'foreign' functons, which are allowed to be declared more
; flexibly in the Haskell runtime. Each such declaration associates a
; builtin to a Haskell function. For these, the naming shceme is
; `unison-FOp-NAME` where `NAME` is the name of the unison builtin
; associated to the declaration.
;
; Both POps and FOps are always called with exactly the right number
; of arguments, so they may be implemented as ordinary scheme
; definitions with a fixed number of arguments. By implementing the
; POp/FOp, you are expecting the associated unison function(s) to be
; implemented by code generation from the wrappers in
; Unison.Runtime.Builtin, so the POp/FOp implementation must
; take/return arguments that match what is expected in those wrappers.

#!r6rs
(library (unison primops)
  (export
    ; unison-FOp-Bytes.decodeNat16be
    ; unison-FOp-Bytes.decodeNat32be
    ; unison-FOp-Bytes.decodeNat64be
    unison-FOp-Char.toText
    ; unison-FOp-Code.dependencies
    ; unison-FOp-Code.serialize
    unison-FOp-IO.closeFile.impl.v3
    unison-FOp-IO.openFile.impl.v3
    unison-FOp-IO.putBytes.impl.v3
    unison-FOp-Text.fromUtf8.impl.v3
    unison-FOp-Text.repeat
    unison-FOp-Text.toUtf8
    ; unison-FOp-Value.serialize
    unison-FOp-IO.stdHandle
    unison-FOp-IO.getArgs.impl.v1

    unison-FOp-ImmutableArray.copyTo!
    unison-FOp-ImmutableArray.read

    unison-FOp-MutableArray.freeze!
    unison-FOp-MutableArray.freeze
    unison-FOp-MutableArray.read
    unison-FOp-MutableArray.write

    unison-FOp-MutableArray.size
    unison-FOp-ImmutableArray.size

    unison-FOp-MutableByteArray.size
    unison-FOp-ImmutableByteArray.size

    unison-FOp-MutableByteArray.length
    unison-FOp-ImmutableByteArray.length

    unison-FOp-ImmutableByteArray.copyTo!
    unison-FOp-ImmutableByteArray.read8

    unison-FOp-MutableByteArray.freeze!
    unison-FOp-MutableByteArray.write8

    unison-FOp-Scope.bytearray
    unison-FOp-Scope.bytearrayOf
    unison-FOp-Scope.array
    unison-FOp-Scope.arrayOf
    unison-FOp-Scope.ref

    unison-FOp-IO.bytearray
    unison-FOp-IO.bytearrayOf
    unison-FOp-IO.array
    unison-FOp-IO.arrayOf

    unison-FOp-IO.ref
    unison-FOp-Ref.read
    unison-FOp-Ref.write
    unison-FOp-Ref.readForCas
    unison-FOp-Ref.Ticket.read
    unison-FOp-Ref.cas

    unison-FOp-Promise.new
    unison-FOp-Promise.read
    unison-FOp-Promise.tryRead
    unison-FOp-Promise.write

    unison-FOp-IO.delay.impl.v3
    unison-POp-FORK
    unison-FOp-IO.kill.impl.v3
    unison-POp-TFRC

    unison-POp-ADDN
    unison-POp-ANDN
    unison-POp-BLDS
    unison-POp-CATS
    unison-POp-CATT
    unison-POp-CMPU
    unison-POp-COMN
    unison-POp-CONS
    unison-POp-DBTX
    unison-POp-DECI
    unison-POp-DIVN
    unison-POp-DRPB
    unison-POp-DRPS
    unison-POp-DRPT
    unison-POp-EQLN
    unison-POp-EQLT
    unison-POp-EQLU
    unison-POp-EROR
    unison-POp-FTOT
    unison-POp-IDXB
    unison-POp-IDXS
    unison-POp-IORN
    unison-POp-ITOT
    unison-POp-LEQN
    ; unison-POp-LKUP
    unison-POp-LZRO
    unison-POp-MULN
    unison-POp-MODN
    unison-POp-NTOT
    unison-POp-PAKT
    unison-POp-SHLI
    unison-POp-SHLN
    unison-POp-SHRI
    unison-POp-SHRN
    unison-POp-SIZS
    unison-POp-SIZT
    unison-POp-SNOC
    unison-POp-SUBN
    unison-POp-TAKS
    unison-POp-TAKT
    unison-POp-TRCE
    unison-POp-TTON
    unison-POp-UPKT
    unison-POp-XORN
    unison-POp-VALU
    unison-POp-VWLS

    unison-POp-UPKB
    unison-POp-PAKB
    unison-POp-ADDI
    unison-POp-DIVI
    unison-POp-EQLI
    unison-POp-MODI
    unison-POp-LEQI
    unison-POp-POWN
    unison-POp-VWRS

    unison-FOp-crypto.hashBytes
    unison-FOp-crypto.hmacBytes
    unison-FOp-crypto.HashAlgorithm.Sha1
    unison-FOp-crypto.HashAlgorithm.Sha2_256
    unison-FOp-crypto.HashAlgorithm.Sha2_512
    unison-FOp-crypto.HashAlgorithm.Sha3_256
    unison-FOp-crypto.HashAlgorithm.Sha3_512
    unison-FOp-crypto.HashAlgorithm.Blake2s_256
    unison-FOp-crypto.HashAlgorithm.Blake2b_256
    unison-FOp-crypto.HashAlgorithm.Blake2b_512

    unison-FOp-IO.clientSocket.impl.v3
    unison-FOp-IO.closeSocket.impl.v3
    unison-FOp-IO.socketReceive.impl.v3
    unison-FOp-IO.socketSend.impl.v3
    unison-FOp-IO.socketPort.impl.v3
    unison-FOp-IO.serverSocket.impl.v3
    unison-FOp-IO.socketAccept.impl.v3
    unison-FOp-IO.listen.impl.v3
    unison-FOp-Tls.ClientConfig.default
    unison-FOp-Tls.ClientConfig.certificates.set
    unison-FOp-Tls.decodeCert.impl.v3
    unison-FOp-Tls.newServer.impl.v3
    unison-FOp-Tls.decodePrivateKey
    unison-FOp-Tls.ServerConfig.default
    unison-FOp-Tls.handshake.impl.v3
    unison-FOp-Tls.newClient.impl.v3
    unison-FOp-Tls.receive.impl.v3
    unison-FOp-Tls.send.impl.v3
    unison-FOp-Tls.terminate.impl.v3
    )

  (import (rnrs)
          (only (srfi :28) format)
          (unison core)
          (unison data)
          (unison data vector-trie)
          (unison string)
          (unison crypto)
          (unison data)
          (unison tls)
          (unison tcp)
          (unison bytevector)
          (unison vector)
          (unison concurrent))

  (define unison-POp-UPKB bytevector->u8-list)
  (define unison-POp-ADDI +)
  (define unison-POp-DIVI /)
  (define (unison-POp-EQLI a b)
    (if (= a b) 1 0)
  )
  (define unison-POp-MODI mod)
  (define unison-POp-LEQI <=)
  (define unison-POp-POWN expt)

  (define (reify-exn thunk)
    (guard
      (e [else
           (sum 0 '() (exception->string e) e)])
      (thunk)))

  ; Core implemented primops, upon which primops-in-unison can be built.
  (define (unison-POp-ADDN m n) (fx+ m n))
  (define (unison-POp-ANDN m n) (fxand m n))
  (define unison-POp-BLDS
    (lambda args-list (fold-right (lambda (e l) (vector-trie-add-first l e)) empty-vector-trie args-list)))
  (define (unison-POp-CATS l r) (append l r))
  (define (unison-POp-CATT l r) (istring-append l r))
  (define (unison-POp-CMPU l r) (universal-compare l r))
  (define (unison-POp-COMN n) (fxnot n))
  (define (unison-POp-CONS x xs) (cons x xs))
  (define (unison-POp-DECI n) (fx1- n))
  (define (unison-POp-DIVN m n) (fxdiv m n))
  (define (unison-POp-DRPB n bs) (ibytevector-drop n bs))
  (define (unison-POp-DRPS n l)
    (let ([m (max 0 (min n (length l)))]) (list-tail l m)))
  (define (unison-POp-DRPT n t) (istring-drop n t))
  (define (unison-POp-EQLN m n) (if (fx=? m n) 1 0))
  (define (unison-POp-EQLT s t) (if (string=? s t) 1 0))
  (define (unison-POp-EQLU x y) (if (universal-equal? x y) 1 0))
  (define (unison-POp-EROR fnm x)
    (let-values ([(p g) (open-string-output-port)])
      (put-string p fnm)
      (put-string p ": ")
      (display x p)
      (raise (g))))
  (define (unison-POp-FTOT f) (number->istring f))
  (define (unison-POp-IDXB n bs) (bytevector-u8-ref bs n))
  (define (unison-POp-IDXS n l)
    (guard (x [else (sum 0)])
      (sum 1 (list-ref l n))))
  (define (unison-POp-IORN m n) (fxior m n))
  (define (unison-POp-ITOT i) (signed-number->istring i))
  (define (unison-POp-LEQN m n) (if (fx<=? m n) 1 0))
  (define (unison-POp-LZRO m) (- 64 (fxlength m)))
  (define (unison-POp-MULN m n) (fx* m n))
  (define (unison-POp-MODN m n) (fxmod m n))
  (define (unison-POp-NTOT m) (number->istring m))
  (define (unison-POp-PAKB l) (u8-list->ibytevector l))
  (define (unison-POp-PAKT l) (list->istring l))
  (define (unison-POp-SHLI i k) (fxarithmetic-shift-left i k))
  (define (unison-POp-SHLN n k) (fxarithmetic-shift-left n k))
  (define (unison-POp-SHRI i k) (fxarithmetic-shift-right i k))
  (define (unison-POp-SHRN n k) (fxarithmetic-shift-right n k))
  (define (unison-POp-SIZS l) (length l))
  (define (unison-POp-SIZT t) (string-length t))
  (define (unison-POp-SNOC xs x) (append xs (list x)))
  (define (unison-POp-SUBN m n) (fx- m n))
  (define (unison-POp-TAKS n s) (list-head s n))
  (define (unison-POp-TAKT n t) (istring-take n t))
  (define (unison-POp-DBTX x)
    (format "~a" x))
  (define (unison-POp-TRCE s x)
    (display s)
    (display "\n")
    (display x)
    (display "\n")
    (display (describe-value x))
    (display "\n"))
  (define (unison-POp-TTON s)
    (let ([mn (string->number s)])
      (if mn (sum 1 mn) (sum 0))))
  (define (unison-POp-UPKT t) (string->list t))
  (define (unison-POp-VWLS l)
    (if (null? l)
      (sum 0)
      (sum 1 (car l) (cdr l))))
  (define (unison-POp-VWRS l)
    (if (null? l)
      (sum 0)
      (let ([r (reverse l)])
      (sum 1 (reverse (cdr l)) (car l)))))

  (define (unison-POp-XORN m n) (fxxor m n))
  (define (unison-POp-VALU c) (decode-value c))

  (define (unison-FOp-IO.putBytes.impl.v3 p bs)
    (begin
      (put-bytevector p bs)
      (flush-output-port p)
      (sum 1 #f)))

  (define (unison-FOp-Char.toText c) (istring c))

  (define stdin (standard-input-port))
  (define stdout (standard-output-port))
  (define stderr (standard-error-port))

  (define (unison-FOp-IO.stdHandle n)
    (case n
      [(0) stdin]
      [(1) stdout]
      [(2) stderr]))

  (define (unison-FOp-IO.getArgs.impl.v1)
    (sum 1 (cdr (command-line))))

  (define (unison-FOp-Text.fromUtf8.impl.v3 s)
    (right (bytevector->string s utf-8-transcoder)))

  (define (unison-FOp-Text.toUtf8 s)
    (string->bytevector s utf-8-transcoder))

  (define (unison-FOp-IO.closeFile.impl.v3 h)
    (close-input-port h))

  (define (unison-FOp-IO.openFile.impl.v3 fn mode)
    (case mode
      [(0) (open-file-input-port fn)]
      [(1) (open-file-output-port fn)]
      [(2) (open-file-output-port fn 'no-truncate)]
      [else (open-file-input/output-port fn)]))

  (define (unison-FOp-Text.repeat n t) (istring-repeat n t))

  (define (catch-array thunk)
    (reify-exn thunk))

  (define (unison-FOp-ImmutableArray.read vec i)
    (catch-array
      (lambda ()
        (sum 1 (vector-ref vec i)))))

  (define (unison-FOp-ImmutableArray.copyTo! dst doff src soff n)
    (catch-array
      (lambda ()
        (let next ([i (fx1- n)])
          (if (< i 0)
            (sum 1 #f)
            (begin
              (vector-set! dst (+ doff i) (vector-ref src (+ soff i)))
              (next (fx1- i))))))))

  (define unison-FOp-MutableArray.freeze! freeze-vector!)

  (define unison-FOp-MutableArray.freeze freeze-subvector)

  (define (unison-FOp-MutableArray.read src i)
    (catch-array
      (lambda ()
        (sum 1 (vector-ref src i)))))

  (define (unison-FOp-MutableArray.write dst i x)
    (catch-array
      (lambda ()
        (vector-set! dst i x)
        (sum 1))))

  (define (unison-FOp-ImmutableByteArray.copyTo! dst doff src soff n)
    (catch-array
      (lambda ()
        (bytevector-copy! src soff dst doff n)
        (sum 1 #f))))

  (define (unison-FOp-ImmutableByteArray.read8 arr i)
    (catch-array
      (lambda ()
        (sum 1 (bytevector-u8-ref arr i)))))

  (define unison-FOp-MutableByteArray.freeze! freeze-bytevector!)

  (define (unison-FOp-MutableByteArray.write8 arr i b)
    (catch-array
      (lambda ()
        (bytevector-u8-set! arr i b)
        (sum 1))))

  (define (unison-FOp-Scope.bytearray n) (make-bytevector n))
  (define (unison-FOp-IO.bytearray n) (make-bytevector n))

  (define (unison-FOp-Scope.array n) (make-vector n))
  (define (unison-FOp-IO.array n) (make-vector n))

  (define (unison-FOp-Scope.bytearrayOf b n) (make-bytevector n b))
  (define (unison-FOp-IO.bytearrayOf b n) (make-bytevector n b))

  (define (unison-FOp-Scope.arrayOf v n) (make-vector n v))
  (define (unison-FOp-IO.arrayOf v n) (make-vector n v))

  (define unison-FOp-MutableByteArray.length bytevector-length)
  (define unison-FOp-ImmutableByteArray.length bytevector-length)
  (define unison-FOp-MutableByteArray.size bytevector-length)
  (define unison-FOp-ImmutableByteArray.size bytevector-length)
  (define unison-FOp-MutableArray.size vector-length)
  (define unison-FOp-ImmutableArray.size vector-length)

  (define (unison-POp-FORK thunk) (fork thunk))
  (define (unison-POp-TFRC thunk) (try-eval thunk))
  (define (unison-FOp-IO.delay.impl.v3 micros) (sleep micros))
  (define (unison-FOp-IO.kill.impl.v3 threadId) (kill threadId))
  (define (unison-FOp-Scope.ref a) (ref-new a))
  (define (unison-FOp-IO.ref a) (ref-new a))
  (define (unison-FOp-Ref.read ref) (ref-read ref))
  (define (unison-FOp-Ref.write ref a) (ref-write ref a))
  (define (unison-FOp-Ref.readForCas ref) (ref-read ref))
  (define (unison-FOp-Ref.Ticket.read ticket) ticket)
  (define (unison-FOp-Ref.cas ref ticket value) (ref-cas ref ticket value))
  (define (unison-FOp-Promise.new) (promise-new))
  (define (unison-FOp-Promise.read promise) (promise-read promise))
  (define (unison-FOp-Promise.tryRead promise) (promise-try-read promise))
  (define (unison-FOp-Promise.write promise a) (promise-write promise a)))

