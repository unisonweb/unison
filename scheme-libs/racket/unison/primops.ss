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

#lang racket/base
(provide
  builtin-Float.*
  builtin-Float.*:termlink
  builtin-Float.>=
  builtin-Float.>=:termlink
  builtin-Float.<=
  builtin-Float.<=:termlink
  builtin-Float.>
  builtin-Float.>:termlink
  builtin-Float.<
  builtin-Float.<:termlink
  builtin-Float.==
  builtin-Float.==:termlink
  builtin-Float.fromRepresentation
  builtin-Float.fromRepresentation:termlink
  builtin-Float.toRepresentation
  builtin-Float.toRepresentation:termlink
  builtin-Float.ceiling
  builtin-Float.ceiling:termlink
  builtin-Float.exp
  builtin-Float.exp:termlink
  builtin-Float.log
  builtin-Float.log:termlink
  builtin-Float.max
  builtin-Float.max:termlink
  builtin-Float.min
  builtin-Float.min:termlink
  builtin-Float.tan
  builtin-Float.tan:termlink
  builtin-Float.tanh
  builtin-Float.tanh:termlink
  builtin-Float.logBase
  builtin-Float.logBase:termlink
  builtin-Float.pow
  builtin-Float.pow:termlink
  builtin-Int.pow
  builtin-Int.pow:termlink
  builtin-Int.*
  builtin-Int.*:termlink
  builtin-Int.+
  builtin-Int.+:termlink
  builtin-Int.-
  builtin-Int.-:termlink
  builtin-Int./
  builtin-Int./:termlink
  builtin-Int.increment
  builtin-Int.increment:termlink
  builtin-Int.negate
  builtin-Int.negate:termlink
  builtin-Int.fromRepresentation
  builtin-Int.fromRepresentation:termlink
  builtin-Int.toRepresentation
  builtin-Int.toRepresentation:termlink
  builtin-Int.signum
  builtin-Int.signum:termlink
  builtin-Int.trailingZeros
  builtin-Int.trailingZeros:termlink
  builtin-Int.popCount
  builtin-Int.popCount:termlink
  builtin-Int.isEven
  builtin-Int.isEven:termlink
  builtin-Int.isOdd
  builtin-Int.isOdd:termlink
  builtin-Int.==
  builtin-Int.==:termlink
  builtin-Int.<
  builtin-Int.<:termlink
  builtin-Int.<=
  builtin-Int.<=:termlink
  builtin-Int.>
  builtin-Int.>:termlink
  builtin-Int.>=
  builtin-Int.>=:termlink
  builtin-Nat.+
  builtin-Nat.+:termlink
  builtin-Nat.drop
  builtin-Nat.drop:termlink
  builtin-Nat.==
  builtin-Nat.==:termlink
  builtin-Nat.<
  builtin-Nat.<:termlink
  builtin-Nat.<=
  builtin-Nat.<=:termlink
  builtin-Nat.>
  builtin-Nat.>:termlink
  builtin-Nat.>=
  builtin-Nat.>=:termlink
  builtin-Nat.isEven
  builtin-Nat.isEven:termlink
  builtin-Nat.isOdd
  builtin-Nat.isOdd:termlink
  builtin-Nat.increment
  builtin-Nat.increment:termlink
  builtin-Nat.popCount
  builtin-Nat.popCount:termlink
  builtin-Nat.toFloat
  builtin-Nat.toFloat:termlink
  builtin-Nat.trailingZeros
  builtin-Nat.trailingZeros:termlink
  builtin-Text.indexOf
  builtin-Text.indexOf:termlink
  builtin-Text.==
  builtin-Text.==:termlink
  builtin-Text.!=
  builtin-Text.!=:termlink
  builtin-Text.<=
  builtin-Text.<=:termlink
  builtin-Text.>=
  builtin-Text.>=:termlink
  builtin-Text.<
  builtin-Text.<:termlink
  builtin-Text.>
  builtin-Text.>:termlink
  builtin-Bytes.indexOf
  builtin-Bytes.indexOf:termlink
  builtin-IO.randomBytes
  builtin-IO.randomBytes:termlink
  builtin-IO.tryEval
  builtin-IO.tryEval:termlink

  builtin-Scope.bytearrayOf
  builtin-Scope.bytearrayOf:termlink

  builtin-Universal.==
  builtin-Universal.==:termlink
  builtin-Universal.>
  builtin-Universal.>:termlink
  builtin-Universal.>=
  builtin-Universal.>=:termlink
  builtin-Universal.<
  builtin-Universal.<:termlink
  builtin-Universal.<=
  builtin-Universal.<=:termlink
  builtin-Universal.compare
  builtin-Universal.compare:termlink
  builtin-Universal.murmurHash:termlink

  builtin-unsafe.coerceAbilities
  builtin-unsafe.coerceAbilities:termlink

  builtin-List.splitLeft
  builtin-List.splitLeft:termlink
  builtin-List.splitRight
  builtin-List.splitRight:termlink

  builtin-Link.Term.toText
  builtin-Link.Term.toText:termlink

  builtin-Value.toBuiltin
  builtin-Value.toBuiltin:termlink
  builtin-Value.fromBuiltin
  builtin-Value.fromBuiltin:termlink
  builtin-Code.fromGroup
  builtin-Code.fromGroup:termlink
  builtin-Code.toGroup
  builtin-Code.toGroup:termlink
  builtin-TermLink.fromReferent
  builtin-TermLink.fromReferent:termlink
  builtin-TermLink.toReferent
  builtin-TermLink.toReferent:termlink
  builtin-TypeLink.toReference
  builtin-TypeLink.toReference:termlink

  builtin-IO.UDP.clientSocket.impl.v1
  builtin-IO.UDP.clientSocket.impl.v1:termlink
  builtin-IO.UDP.UDPSocket.recv.impl.v1
  builtin-IO.UDP.UDPSocket.recv.impl.v1:termlink
  builtin-IO.UDP.UDPSocket.send.impl.v1
  builtin-IO.UDP.UDPSocket.send.impl.v1:termlink
  builtin-IO.UDP.UDPSocket.close.impl.v1
  builtin-IO.UDP.UDPSocket.close.impl.v1:termlink
  builtin-IO.UDP.ListenSocket.close.impl.v1
  builtin-IO.UDP.ListenSocket.close.impl.v1:termlink
  builtin-IO.UDP.UDPSocket.toText.impl.v1
  builtin-IO.UDP.UDPSocket.toText.impl.v1:termlink
  builtin-IO.UDP.serverSocket.impl.v1
  builtin-IO.UDP.serverSocket.impl.v1:termlink
  builtin-IO.UDP.ListenSocket.toText.impl.v1
  builtin-IO.UDP.ListenSocket.toText.impl.v1:termlink
  builtin-IO.UDP.ListenSocket.recvFrom.impl.v1
  builtin-IO.UDP.ListenSocket.recvFrom.impl.v1:termlink
  builtin-IO.UDP.ClientSockAddr.toText.v1
  builtin-IO.UDP.ClientSockAddr.toText.v1:termlink
  builtin-IO.UDP.ListenSocket.sendTo.impl.v1
  builtin-IO.UDP.ListenSocket.sendTo.impl.v1:termlink

  unison-FOp-internal.dataTag
  unison-FOp-Char.toText
  ; unison-FOp-Code.dependencies
  ; unison-FOp-Code.serialize
  unison-FOp-IO.closeFile.impl.v3
  unison-FOp-IO.openFile.impl.v3
  ; unison-FOp-IO.isFileEOF.impl.v3
  unison-FOp-IO.putBytes.impl.v3
  unison-FOp-IO.getBytes.impl.v3
  builtin-IO.seekHandle.impl.v3
  builtin-IO.seekHandle.impl.v3:termlink
  builtin-IO.getLine.impl.v1
  builtin-IO.getLine.impl.v1:termlink
  builtin-IO.getSomeBytes.impl.v1
  builtin-IO.getSomeBytes.impl.v1:termlink
  builtin-IO.setBuffering.impl.v3
  builtin-IO.setBuffering.impl.v3:termlink
  builtin-IO.getBuffering.impl.v3
  builtin-IO.getBuffering.impl.v3:termlink
  builtin-IO.setEcho.impl.v1
  builtin-IO.setEcho.impl.v1:termlink
  builtin-IO.isFileOpen.impl.v3
  builtin-IO.isFileOpen.impl.v3:termlink
  builtin-IO.ready.impl.v1
  builtin-IO.ready.impl.v1:termlink
  builtin-IO.process.call
  builtin-IO.process.call:termlink
  builtin-IO.getEcho.impl.v1
  builtin-IO.getEcho.impl.v1:termlink
  builtin-IO.getArgs.impl.v1
  builtin-IO.getArgs.impl.v1:termlink
  builtin-IO.getEnv.impl.v1
  builtin-IO.getEnv.impl.v1:termlink
  builtin-IO.getChar.impl.v1
  builtin-IO.getChar.impl.v1:termlink
  builtin-IO.getCurrentDirectory.impl.v3
  builtin-IO.getCurrentDirectory.impl.v3:termlink
  builtin-IO.removeDirectory.impl.v3
  builtin-IO.removeDirectory.impl.v3:termlink
  builtin-IO.renameFile.impl.v3
  builtin-IO.renameFile.impl.v3:termlink
  builtin-IO.createTempDirectory.impl.v3
  builtin-IO.createTempDirectory.impl.v3:termlink
  builtin-IO.createDirectory.impl.v3
  builtin-IO.createDirectory.impl.v3:termlink
  builtin-IO.setCurrentDirectory.impl.v3
  builtin-IO.setCurrentDirectory.impl.v3:termlink
  builtin-IO.renameDirectory.impl.v3
  builtin-IO.renameDirectory.impl.v3:termlink
  builtin-IO.isDirectory.impl.v3
  builtin-IO.isDirectory.impl.v3:termlink
  builtin-IO.isSeekable.impl.v3
  builtin-IO.isSeekable.impl.v3:termlink
  builtin-IO.handlePosition.impl.v3
  builtin-IO.handlePosition.impl.v3:termlink
  builtin-IO.systemTime.impl.v3
  builtin-IO.systemTime.impl.v3:termlink
  builtin-IO.systemTimeMicroseconds.impl.v3
  builtin-IO.systemTimeMicroseconds.impl.v3:termlink

  builtin-Char.Class.is
  builtin-Char.Class.is:termlink
  builtin-Pattern.captureAs
  builtin-Pattern.captureAs:termlink
  builtin-Pattern.many.corrected
  builtin-Pattern.many.corrected:termlink
  builtin-Pattern.isMatch
  builtin-Pattern.isMatch:termlink
  builtin-IO.fileExists.impl.v3
  builtin-IO.fileExists.impl.v3:termlink
  builtin-IO.isFileEOF.impl.v3
  builtin-IO.isFileEOF.impl.v3:termlink

  unison-FOp-IO.getFileSize.impl.v3
  unison-FOp-IO.getFileTimestamp.impl.v3
  ; unison-FOp-IO.fileExists.impl.v3
  unison-FOp-IO.removeFile.impl.v3
  unison-FOp-IO.getTempDirectory.impl.v3
  unison-FOp-Text.fromUtf8.impl.v3
  unison-FOp-Text.repeat
  unison-FOp-Text.reverse
  unison-FOp-Text.toUtf8
  unison-FOp-Text.toLowercase
  unison-FOp-Text.toUppercase
  unison-FOp-Pattern.run
  unison-FOp-Pattern.isMatch
  unison-FOp-Pattern.many
  unison-FOp-Pattern.capture
  unison-FOp-Pattern.join
  unison-FOp-Pattern.or
  unison-FOp-Pattern.replicate
  unison-FOp-Text.patterns.digit
  unison-FOp-Text.patterns.letter
  unison-FOp-Text.patterns.punctuation
  unison-FOp-Text.patterns.charIn
  unison-FOp-Text.patterns.notCharIn
  unison-FOp-Text.patterns.anyChar
  unison-FOp-Text.patterns.space
  unison-FOp-Text.patterns.charRange
  unison-FOp-Text.patterns.notCharRange
  unison-FOp-Text.patterns.literal
  unison-FOp-Text.patterns.eof
  unison-FOp-Text.patterns.char
  unison-FOp-Char.Class.is
  unison-FOp-Char.Class.any
  unison-FOp-Char.Class.alphanumeric
  unison-FOp-Char.Class.upper
  unison-FOp-Char.Class.lower
  unison-FOp-Char.Class.number
  unison-FOp-Char.Class.punctuation
  unison-FOp-Char.Class.symbol
  unison-FOp-Char.Class.letter
  unison-FOp-Char.Class.whitespace
  unison-FOp-Char.Class.control
  unison-FOp-Char.Class.printable
  unison-FOp-Char.Class.mark
  unison-FOp-Char.Class.separator
  unison-FOp-Char.Class.or
  unison-FOp-Char.Class.range
  unison-FOp-Char.Class.anyOf
  unison-FOp-Char.Class.and
  unison-FOp-Char.Class.not
  unison-FOp-Clock.internals.nsec.v1
  unison-FOp-Clock.internals.sec.v1
  unison-FOp-Clock.internals.threadCPUTime.v1
  unison-FOp-Clock.internals.processCPUTime.v1
  unison-FOp-Clock.internals.realtime.v1
  unison-FOp-Clock.internals.monotonic.v1
  builtin-Clock.internals.systemTimeZone.v1
  builtin-Clock.internals.systemTimeZone.v1:termlink


  ; unison-FOp-Value.serialize
  unison-FOp-IO.stdHandle
  unison-FOp-IO.getArgs.impl.v1

  builtin-IO.directoryContents.impl.v3
  builtin-IO.directoryContents.impl.v3:termlink
  unison-FOp-IO.systemTimeMicroseconds.v1

  unison-FOp-ImmutableArray.copyTo!
  unison-FOp-ImmutableArray.read

  unison-FOp-MutableArray.copyTo!
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
  unison-FOp-ImmutableByteArray.read16be
  unison-FOp-ImmutableByteArray.read24be
  unison-FOp-ImmutableByteArray.read32be
  unison-FOp-ImmutableByteArray.read40be
  unison-FOp-ImmutableByteArray.read48be
  unison-FOp-ImmutableByteArray.read56be
  unison-FOp-ImmutableByteArray.read64be

  unison-FOp-MutableByteArray.copyTo!
  unison-FOp-MutableByteArray.freeze!
  unison-FOp-MutableByteArray.write8
  unison-FOp-MutableByteArray.write16be
  unison-FOp-MutableByteArray.write32be
  unison-FOp-MutableByteArray.write64be
  unison-FOp-MutableByteArray.read8
  unison-FOp-MutableByteArray.read16be
  unison-FOp-MutableByteArray.read24be
  unison-FOp-MutableByteArray.read32be
  unison-FOp-MutableByteArray.read40be
  unison-FOp-MutableByteArray.read64be

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

  unison-FOp-Handle.toText
  unison-FOp-Socket.toText
  unison-FOp-ThreadId.toText

  unison-POp-ABSF
  unison-POp-ACOS
  unison-POp-ACSH
  unison-POp-ADDF
  unison-POp-ASIN
  unison-POp-ASNH
  unison-POp-ATAN
  unison-POp-ATN2
  unison-POp-ATNH
  unison-POp-CEIL
  unison-POp-FLOR
  unison-POp-COSF
  unison-POp-COSH
  unison-POp-DIVF
  unison-POp-DIVI
  unison-POp-EQLF
  unison-POp-EQLI
  unison-POp-SUBF
  unison-POp-SUBI
  unison-POp-SGNI
  unison-POp-LEQF
  unison-POp-SINF
  unison-POp-SINH
  unison-POp-TRNF
  unison-POp-RNDF
  unison-POp-SQRT
  unison-POp-TANH
  unison-POp-TANF
  unison-POp-TZRO
  unison-POp-POPC
  unison-POp-ITOF

  unison-POp-ADDN
  unison-POp-ANDN
  unison-POp-BLDS
  unison-POp-CATS
  unison-POp-CATT
  unison-POp-CATB
  unison-POp-CMPU
  unison-POp-COMN
  unison-POp-CONS
  unison-POp-DBTX
  unison-POp-DECI
  unison-POp-INCI
  unison-POp-DECN
  unison-POp-INCN
  unison-POp-DIVN
  unison-POp-DRPB
  unison-POp-DRPS
  unison-POp-DRPT
  unison-POp-EQLN
  unison-POp-EQLT
  unison-POp-EXPF
  unison-POp-LEQT
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
  unison-POp-SIZB
  unison-POp-SNOC
  unison-POp-SUBN
  unison-POp-SUBI
  unison-POp-TAKS
  unison-POp-TAKT
  unison-POp-TAKB
  unison-POp-TRCE
  unison-POp-PRNT
  unison-POp-TTON
  unison-POp-TTOI
  unison-POp-TTOF
  unison-POp-UPKT
  unison-POp-XORN
  unison-POp-VALU
  unison-POp-VWLS
  unison-POp-UCNS
  unison-POp-USNC
  unison-POp-FLTB
  unison-POp-MAXF
  unison-POp-MINF
  unison-POp-MULF
  unison-POp-MULI
  unison-POp-NEGI
  unison-POp-NTOF
  unison-POp-POWF
  unison-POp-POWI
  unison-POp-POWN

  unison-POp-UPKB
  unison-POp-PAKB
  unison-POp-ADDI
  unison-POp-MULI
  unison-POp-MODI
  unison-POp-LEQI
  unison-POp-LOGB
  unison-POp-LOGF
  unison-POp-POWN
  unison-POp-VWRS
  unison-POp-SPLL
  unison-POp-SPLR

  unison-FOp-Bytes.gzip.compress
  unison-FOp-Bytes.gzip.decompress
  unison-FOp-Bytes.zlib.compress
  unison-FOp-Bytes.zlib.decompress
  unison-FOp-Bytes.toBase16
  unison-FOp-Bytes.toBase32
  unison-FOp-Bytes.toBase64
  unison-FOp-Bytes.toBase64UrlUnpadded
  unison-FOp-Bytes.fromBase16
  unison-FOp-Bytes.fromBase32
  unison-FOp-Bytes.fromBase64
  unison-FOp-Bytes.fromBase64UrlUnpadded
  unison-FOp-Bytes.encodeNat16be
  unison-FOp-Bytes.encodeNat16le
  unison-FOp-Bytes.encodeNat32be
  unison-FOp-Bytes.encodeNat32le
  unison-FOp-Bytes.encodeNat64be
  unison-FOp-Bytes.encodeNat64le
  unison-FOp-Bytes.decodeNat16be
  unison-FOp-Bytes.decodeNat16le
  unison-FOp-Bytes.decodeNat32be
  unison-FOp-Bytes.decodeNat32le
  unison-FOp-Bytes.decodeNat64be
  unison-FOp-Bytes.decodeNat64le

  unison-FOp-crypto.hashBytes
  unison-FOp-crypto.hmacBytes
  unison-FOp-crypto.HashAlgorithm.Md5
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
  unison-FOp-Tls.encodeCert
  unison-FOp-Tls.newServer.impl.v3
  unison-FOp-Tls.decodePrivateKey
  unison-FOp-Tls.encodePrivateKey
  unison-FOp-Tls.ServerConfig.default
  unison-FOp-Tls.handshake.impl.v3
  unison-FOp-Tls.newClient.impl.v3
  unison-FOp-Tls.receive.impl.v3
  unison-FOp-Tls.send.impl.v3
  unison-FOp-Tls.terminate.impl.v3

  ; fake builtins
  builtin-murmurHashBytes)

(require
  (except-in racket
    eof
    sleep)

  (only-in srfi/13 string-reverse)
  rnrs/bytevectors-6

  racket/performance-hint

  (only-in racket/flonum
    fl<
    fl>
    fl<=
    fl>=
    fl=)

  (only-in racket/string
    string-contains?
    string-replace)

  unison/arithmetic
  unison/bytevector
  unison/core

  (only-in unison/boot
    define-unison-builtin
    referent->termlink
    termlink->referent
    typelink->reference
    clamp-integer
    clamp-natural
    wrap-natural
    exn:bug->exception
    raise-unison-exception
    bit64
    bit63
    nbit63)

  unison/data
  unison/data-info
  unison/math
  unison/chunked-seq
  unison/chunked-bytes
  unison/string-search
  unison/bytes-nat
  unison/pattern
  unison/crypto
  unison/io
  unison/io-handles
  unison/murmurhash
  unison/tls
  unison/tcp
  unison/udp
  unison/gzip
  unison/zlib
  unison/concurrent
  racket/random)

; (define-builtin-link Float.*)
; (define-builtin-link Float.fromRepresentation)
; (define-builtin-link Float.toRepresentation)
; (define-builtin-link Float.ceiling)
; (define-builtin-link Float.exp)
; (define-builtin-link Float.log)
; (define-builtin-link Float.max)
; (define-builtin-link Float.min)
; (define-builtin-link Float.tan)
; (define-builtin-link Float.tanh)
; (define-builtin-link Float.logBase)
; (define-builtin-link Float.pow)
; (define-builtin-link Float.>)
; (define-builtin-link Float.<)
; (define-builtin-link Float.>=)
; (define-builtin-link Float.<=)
; (define-builtin-link Float.==)
; (define-builtin-link Int.pow)
; (define-builtin-link Int.*)
; (define-builtin-link Int.+)
; (define-builtin-link Int.-)
; (define-builtin-link Int./)
; (define-builtin-link Int.>)
; (define-builtin-link Int.<)
; (define-builtin-link Int.>=)
; (define-builtin-link Int.<=)
; (define-builtin-link Int.==)
; (define-builtin-link Int.isEven)
; (define-builtin-link Int.isOdd)
; (define-builtin-link Int.increment)
; (define-builtin-link Int.negate)
; (define-builtin-link Int.fromRepresentation)
; (define-builtin-link Int.toRepresentation)
; (define-builtin-link Int.signum)
; (define-builtin-link Int.trailingZeros)
; (define-builtin-link Int.popCount)
; (define-builtin-link Nat.increment)
; (define-builtin-link Nat.popCount)
; (define-builtin-link Nat.toFloat)
; (define-builtin-link Nat.trailingZeros)
; (define-builtin-link Nat.+)
; (define-builtin-link Nat.>)
; (define-builtin-link Nat.<)
; (define-builtin-link Nat.>=)
; (define-builtin-link Nat.<=)
; (define-builtin-link Nat.==)
; (define-builtin-link Nat.drop)
; (define-builtin-link Nat.isEven)
; (define-builtin-link Nat.isOdd)
; (define-builtin-link Text.indexOf)
; (define-builtin-link Text.>)
; (define-builtin-link Text.<)
; (define-builtin-link Text.>=)
; (define-builtin-link Text.<=)
; (define-builtin-link Text.==)
; (define-builtin-link Text.!=)
; (define-builtin-link Bytes.indexOf)
; (define-builtin-link IO.randomBytes)
; (define-builtin-link IO.tryEval)
; (define-builtin-link List.splitLeft)
; (define-builtin-link List.splitRight)
; (define-builtin-link Value.toBuiltin)
; (define-builtin-link Value.fromBuiltin)
; (define-builtin-link Code.fromGroup)
; (define-builtin-link Code.toGroup)
; (define-builtin-link TermLink.fromReferent)
; (define-builtin-link TermLink.toReferent)
; (define-builtin-link TypeLink.toReference)
; (define-builtin-link IO.seekHandle.impl.v3)
; (define-builtin-link IO.getLine.impl.v1)
; (define-builtin-link IO.getSomeBytes.impl.v1)
; (define-builtin-link IO.setBuffering.impl.v3)
; (define-builtin-link IO.getBuffering.impl.v3)
; (define-builtin-link IO.setEcho.impl.v1)
; (define-builtin-link IO.isFileOpen.impl.v3)
; (define-builtin-link IO.ready.impl.v1)
; (define-builtin-link IO.process.call)
; (define-builtin-link IO.getEcho.impl.v1)
; (define-builtin-link IO.getArgs.impl.v1)
; (define-builtin-link IO.getEnv.impl.v1)
; (define-builtin-link IO.getChar.impl.v1)
; (define-builtin-link IO.getCurrentDirectory.impl.v3)
; (define-builtin-link IO.directoryContents.impl.v3)
; (define-builtin-link IO.removeDirectory.impl.v3)
; (define-builtin-link IO.renameFile.impl.v3)
; (define-builtin-link IO.createTempDirectory.impl.v3)
; (define-builtin-link IO.createDirectory.impl.v3)
; (define-builtin-link IO.setCurrentDirectory.impl.v3)
; (define-builtin-link IO.renameDirectory.impl.v3)
; (define-builtin-link IO.fileExists.impl.v3)
; (define-builtin-link IO.isDirectory.impl.v3)
; (define-builtin-link IO.isFileEOF.impl.v3)
; (define-builtin-link IO.isSeekable.impl.v3)
; (define-builtin-link IO.handlePosition.impl.v3)
; (define-builtin-link IO.systemTime.impl.v3)
; (define-builtin-link IO.systemTimeMicroseconds.impl.v3)
; (define-builtin-link Universal.==)
; (define-builtin-link Universal.>)
; (define-builtin-link Universal.<)
; (define-builtin-link Universal.>=)
; (define-builtin-link Universal.<=)
; (define-builtin-link Universal.compare)
(define-builtin-link Universal.murmurHash)
; (define-builtin-link Pattern.captureAs)
; (define-builtin-link Pattern.many.corrected)
; (define-builtin-link Pattern.isMatch)
; (define-builtin-link Char.Class.is)
; (define-builtin-link Scope.bytearrayOf)
; (define-builtin-link unsafe.coerceAbilities)
(define-builtin-link Clock.internals.systemTimeZone.v1)

(begin-encourage-inline
  (define-unison-builtin (builtin-Value.toBuiltin v) (unison-quote v))
  (define-unison-builtin (builtin-Value.fromBuiltin v)
    (unison-quote-val v))
  (define-unison-builtin (builtin-Code.fromGroup sg) (unison-code sg))
  (define-unison-builtin (builtin-Code.toGroup co)
    (unison-code-rep co))
  (define-unison-builtin (builtin-TermLink.fromReferent rf)
    (referent->termlink rf))
  (define-unison-builtin (builtin-TermLink.toReferent tl)
    (termlink->referent tl))
  (define-unison-builtin (builtin-TypeLink.toReference tl)
    (typelink->reference tl))
  (define-unison-builtin (builtin-murmurHashBytes bs)
    (murmurhash-bytes (chunked-bytes->bytes bs)))

  (define-unison-builtin (builtin-IO.randomBytes n)
    (bytes->chunked-bytes (crypto-random-bytes n)))

  (define-unison-builtin (builtin-List.splitLeft n s)
    (match (unison-POp-SPLL n s)
      [(unison-sum 0 fs) ref-seqview-empty]
      [(unison-sum 1 (list l r)) (ref-seqview-elem l r)]))

  (define-unison-builtin (builtin-List.splitRight n s)
    (match (unison-POp-SPLR n s)
      [(unison-sum 0 fs) ref-seqview-empty]
      [(unison-sum 1 (list l r)) (ref-seqview-elem l r)]))

  (define-unison-builtin (builtin-Float.> x y) (fl> x y))
  (define-unison-builtin (builtin-Float.< x y) (fl< x y))
  (define-unison-builtin (builtin-Float.>= x y) (fl>= x y))
  (define-unison-builtin (builtin-Float.<= x y) (fl<= x y))
  (define-unison-builtin (builtin-Float.== x y) (fl= x y))

  (define-unison-builtin (builtin-Int.> x y) (> x y))
  (define-unison-builtin (builtin-Int.< x y) (< x y))
  (define-unison-builtin (builtin-Int.>= x y) (>= x y))
  (define-unison-builtin (builtin-Int.<= x y) (<= x y))
  (define-unison-builtin (builtin-Int.== x y) (= x y))
  (define-unison-builtin (builtin-Int.isEven x) (even? x))
  (define-unison-builtin (builtin-Int.isOdd x) (odd? x))

  (define-unison-builtin (builtin-Nat.> x y) (> x y))
  (define-unison-builtin (builtin-Nat.< x y) (< x y))
  (define-unison-builtin (builtin-Nat.>= x y) (>= x y))
  (define-unison-builtin (builtin-Nat.<= x y) (<= x y))
  (begin-encourage-inline
    (define-unison-builtin (builtin-Nat.== x y) (= x y)))

  (define-unison-builtin (builtin-Nat.isEven x) (even? x))
  (define-unison-builtin (builtin-Nat.isOdd x) (odd? x))

  ; Note: chunked-string<? is actually <=
  (define-unison-builtin (builtin-Text.> x y)
    (not (chunked-string<? x y)))
  (define-unison-builtin (builtin-Text.< x y)
    (not (chunked-string<? y x)))
  (define-unison-builtin (builtin-Text.>= x y) (chunked-string<? y x))
  (define-unison-builtin (builtin-Text.<= x y) (chunked-string<? x y))
  (define-unison-builtin (builtin-Text.== x y) (equal? x y))
  (define-unison-builtin (builtin-Text.!= x y) (not (equal? x y)))

  (define-unison-builtin (builtin-Universal.== x y) (universal=? x y))
  (define-unison-builtin (builtin-Universal.> x y)
    (case (universal-compare x y) [(>) #t] [else #f]))
  (define-unison-builtin (builtin-Universal.< x y)
    (case (universal-compare x y) [(<) #t] [else #f]))
  (define-unison-builtin (builtin-Universal.<= x y)
    (case (universal-compare x y) [(>) #f] [else #t]))
  (define-unison-builtin (builtin-Universal.>= x y)
    (case (universal-compare x y) [(<) #f] [else #t]))
  (define-unison-builtin (builtin-Universal.compare x y)
    (case (universal-compare x y)
      [(>) 1] [(<) -1] [else 0]))

  (define-unison-builtin (builtin-Scope.bytearrayOf i n)
    (make-bytes n i))

  ; (define-builtin-link Link.Type.toText)
  (define-unison-builtin (builtin-Link.Type.toText ln)
    (string->chunked-string (typelink->string ln)))

  ; (define-builtin-link Link.Term.toText)
  (define-unison-builtin (builtin-Link.Term.toText ln)
    (string->chunked-string (termlink->string ln)))

  (define-unison-builtin (builtin-Char.Class.is cc c)
    (pattern-match? cc (string->chunked-string (string c))))

  (define-unison-builtin (builtin-Pattern.captureAs c p)
    (capture-as c p))

  (define-unison-builtin (builtin-Pattern.many.corrected p) (many p))

  (define-unison-builtin (builtin-Pattern.isMatch p s)
    (pattern-match? p s))

  (define-unison-builtin (builtin-unsafe.coerceAbilities f) f)

  (define (unison-POp-UPKB bs)
    (build-chunked-list
     (chunked-bytes-length bs)
     (lambda (i) (chunked-bytes-ref bs i))))

  (define (unison-POp-ADDI i j) (clamp-integer (+ i j)))
  (define (unison-POp-MULI i j) (clamp-integer (* i j)))
  (define (unison-POp-MODI i j) (clamp-integer (modulo i j)))
  (define (unison-POp-LEQI a b) (bool (<= a b)))
  (define (unison-POp-POWN m n) (clamp-natural (expt m n)))
  (define unison-POp-LOGF log)

  (define (reify-exn thunk)
    (with-handlers
      ([exn:fail:contract?
         (lambda (e)
           (sum 0 '() (exception->string e) ref-unit-unit))])
      (thunk)))

  ; Core implemented primops, upon which primops-in-unison can be built.
  (define (unison-POp-ADDN m n) (clamp-natural (+ m n)))
  (define (unison-POp-ANDN m n) (bitwise-and m n))
  (define unison-POp-BLDS
    (lambda args-list
      (foldr (lambda (e l) (chunked-list-add-first l e)) empty-chunked-list args-list)))
  (define (unison-POp-CATS l r) (chunked-list-append l r))
  (define (unison-POp-CATT l r) (chunked-string-append l r))
  (define (unison-POp-CATB l r) (chunked-bytes-append l r))
  (define (unison-POp-CMPU l r) (ord (universal-compare l r)))
  (define (unison-POp-COMN n) (wrap-natural (bitwise-not n)))
  (define (unison-POp-CONS x xs) (chunked-list-add-first xs x))
  (define (unison-POp-DECI n) (clamp-integer (sub1 n)))
  (define (unison-POp-INCI n) (clamp-integer (add1 n)))
  (define (unison-POp-DECN n) (wrap-natural (sub1 n)))
  (define (unison-POp-INCN n) (clamp-natural (add1 n)))
  (define (unison-POp-DIVN m n) (quotient m n))
  (define (unison-POp-DRPB n bs) (chunked-bytes-drop bs n))
  (define (unison-POp-DRPS n l) (chunked-list-drop l n))
  (define (unison-POp-DRPT n t) (chunked-string-drop t n))
  (define (unison-POp-EQLN m n) (bool (= m n)))
  (define (unison-POp-EQLT s t) (bool (equal? s t)))
  (define (unison-POp-LEQT s t) (bool (chunked-string<? s t)))
  (define (unison-POp-EQLU x y) (bool (universal=? x y)))
  (define (unison-POp-EROR fnm x)
    (let ([snm (chunked-string->string fnm)])
      (raise (make-exn:bug snm x))))
  (define (unison-POp-FTOT f)
    (define base (number->string f))
    (define dotted
      (if (string-contains? base ".")
        base
        (string-replace base "e" ".0e")))
    (string->chunked-string
      (string-replace dotted "+" "")))
  (define (unison-POp-IDXB n bs)
    (with-handlers
      ([exn:fail:contract? (lambda (e) none)])
      (some (chunked-bytes-ref bs n))))
  (define (unison-POp-IDXS n l)
    (with-handlers
      ([exn:fail:contract? (lambda (x) none)])
      (some (chunked-list-ref l n))))
  (define (unison-POp-IORN m n) (bitwise-ior m n))
  (define (unison-POp-ITOT n)
    (string->chunked-string (number->string n)))
  (define (unison-POp-LEQN m n) (bool (<= m n)))
  (define (unison-POp-LZRO m) (- 64 (integer-length m)))
  (define (unison-POp-MULN m n) (clamp-natural (* m n)))
  (define (unison-POp-MODN m n) (modulo m n))
  (define (unison-POp-NTOT n) (string->chunked-string (number->string n)))
  (define (unison-POp-PAKB l)
    (build-chunked-bytes
     (chunked-list-length l)
     (lambda (i) (chunked-list-ref l i))))
  (define (unison-POp-PAKT l)
    (build-chunked-string
     (chunked-list-length l)
     (lambda (i) (chunked-list-ref l i))))
  (define (unison-POp-SHLI i k)
    (clamp-integer (arithmetic-shift i k)))
  (define (unison-POp-SHLN n k)
    (clamp-natural (arithmetic-shift n k)))
  (define (unison-POp-SHRI i k) (arithmetic-shift i (- k)))
  (define (unison-POp-SHRN n k) (arithmetic-shift n (- k)))
  (define (unison-POp-SIZS l) (chunked-list-length l))
  (define (unison-POp-SIZT t) (chunked-string-length t))
  (define (unison-POp-SIZB b) (chunked-bytes-length b))
  (define (unison-POp-SNOC xs x) (chunked-list-add-last xs x))
  (define (unison-POp-SUBN m n) (clamp-integer (- m n)))
  (define (unison-POp-SUBI m n) (clamp-integer (- m n)))
  (define (unison-POp-TAKS n s) (chunked-list-take s n))
  (define (unison-POp-TAKT n t) (chunked-string-take t n))
  (define (unison-POp-TAKB n t) (chunked-bytes-take t n))

  (define (->optional v)
    (if v
        (ref-optional-some v)
        ref-optional-none))

  (define-unison-builtin (builtin-Text.indexOf n h)
    (->optional (chunked-string-index-of h n)))
  (define-unison-builtin (builtin-Bytes.indexOf n h)
    (->optional (chunked-bytes-index-of h n)))

  ;; TODO currently only runs in low-level tracing support
  (define (unison-POp-DBTX x)
    (sum 1 (string->chunked-string (describe-value x))))

  (define (unison-FOp-Handle.toText h)
    (string->chunked-string (describe-value h)))
  (define (unison-FOp-Socket.toText s)
    (string->chunked-string (describe-value s)))
  (define (unison-FOp-ThreadId.toText tid)
    (string->chunked-string (describe-value tid)))

  (define (unison-POp-TRCE s x)
    (display "trace: ")
    (display (chunked-string->string s))
    (newline)
    (display (describe-value x))
    (newline))
  (define (unison-POp-PRNT s)
    (display (chunked-string->string s))
    (newline))
  (define (unison-POp-TTON s)
    (let ([mn (string->number (chunked-string->string s))])
      (if (and (exact-nonnegative-integer? mn) (< mn bit64))
        (some mn)
        none)))
  (define (unison-POp-TTOI s)
    (let ([mn (string->number (chunked-string->string s))])
      (if (and (exact-integer? mn) (>= mn nbit63) (< mn bit63))
        (some mn)
        none)))
  (define (unison-POp-TTOF s)
    (let ([mn (string->number (chunked-string->string s))])
      (if mn (some mn) none)))
  (define (unison-POp-UPKT s)
    (build-chunked-list
     (chunked-string-length s)
     (lambda (i) (chunked-string-ref s i))))
  (define (unison-POp-VWLS l)
    (if (chunked-list-empty? l)
        (sum 0)
        (let-values ([(t h) (chunked-list-pop-first l)])
          (sum 1 h t))))
  (define (unison-POp-VWRS l)
    (if (chunked-list-empty? l)
        (sum 0)
        (let-values ([(t h) (chunked-list-pop-last l)])
          (sum 1 t h))))
  (define (unison-POp-SPLL i s)
    (if (< (chunked-list-length s) i)
        (sum 0)
        (let-values ([(l r) (chunked-list-split-at s i)])
          (sum 1 l r))))
  (define (unison-POp-SPLR i s) ; TODO write test that stresses this
    (let ([len (chunked-list-length s) ])
      (if (< len i)
          (sum 0)
          (let-values ([(l r) (chunked-list-split-at s (- len i))])
            (sum 1 l r)))))

  (define (unison-POp-UCNS s)
    (if (chunked-string-empty? s)
        (sum 0)
        (let-values ([(t h) (chunked-string-pop-first s)])
          (sum 1 (char h) t))))

  (define (unison-POp-USNC s)
    (if (chunked-string-empty? s)
        (sum 0)
        (let-values ([(t h) (chunked-string-pop-last s)])
          (sum 1 t (char h)))))

  ;; TODO flatten operation on Bytes is a no-op for now (and possibly ever)
  (define (unison-POp-FLTB b) b)

  (define (unison-POp-XORN m n) (bitwise-xor m n))
  (define (unison-POp-VALU c) (decode-value c))

  (define (unison-FOp-ImmutableByteArray.read16be bs n)
    (reify-exn
      (lambda ()
        (sum 1 (bytevector-u16-ref bs n 'big)))))

  (define (unison-FOp-ImmutableByteArray.read24be bs n)
    (reify-exn
      (lambda ()
        (sum 1 (bytevector-u24-ref bs n 'big)))))

  (define (unison-FOp-ImmutableByteArray.read32be bs n)
    (reify-exn
      (lambda ()
        (sum 1 (bytevector-u32-ref bs n 'big)))))

  (define (unison-FOp-ImmutableByteArray.read40be bs n)
    (reify-exn
      (lambda ()
        (sum 1 (bytevector-u40-ref bs n 'big)))))

  (define (unison-FOp-ImmutableByteArray.read48be bs n)
    (reify-exn
      (lambda ()
        (sum 1 (bytevector-u48-ref bs n 'big)))))

  (define (unison-FOp-ImmutableByteArray.read56be bs n)
    (reify-exn
      (lambda ()
        (sum 1 (bytevector-u56-ref bs n 'big)))))

  (define (unison-FOp-ImmutableByteArray.read64be bs n)
    (reify-exn
      (lambda ()
        (sum 1 (bytevector-u64-ref bs n 'big)))))

  (define unison-FOp-internal.dataTag unison-data-tag)

  (define (unison-FOp-IO.getBytes.impl.v3 p n)
    (reify-exn
      (lambda ()
        (right
          (bytes->chunked-bytes
            (read-bytes n p))))))

  (define (unison-FOp-IO.putBytes.impl.v3 p bs)
    (begin
      (write-bytes (chunked-bytes->bytes bs) p)
      (flush-output p)
      (sum 1 #f)))

  (define (unison-FOp-Char.toText c) (string->chunked-string (string (integer->char c))))

  (define (unison-FOp-IO.getArgs.impl.v1)
    (sum 1 (cdr (command-line))))

  (define unison-FOp-IO.systemTimeMicroseconds.v1 current-microseconds)

  ;; TODO should we convert Bytes -> Text directly without the intermediate conversions?
  (define (unison-FOp-Text.fromUtf8.impl.v3 b)
    (with-handlers
      ([exn:fail:contract?
        (lambda (e)
          (exception
            ref-iofailure:typelink
            (string->chunked-string
              (string-append
                "Invalid UTF-8 stream: "
                (describe-value b)))
            (exception->string e)))])
      (right (string->chunked-string (bytes->string/utf-8 (chunked-bytes->bytes b))))))

  ;; TODO should we convert Text -> Bytes directly without the intermediate conversions?
  (define (unison-FOp-Text.toUtf8 s)
    (bytes->chunked-bytes (string->bytes/utf-8 (chunked-string->string s))))

  (define-unison-builtin (builtin-IO.isFileEOF.impl.v3 p)
    (ref-either-right (eof-object? (peek-byte p))))

  (define (unison-FOp-IO.closeFile.impl.v3 h)
    (if (input-port? h)
        (close-input-port h)
        (close-output-port h))
    (right none))

  (define (unison-FOp-Text.repeat n t)
    (let loop ([cnt 0]
               [acc empty-chunked-string])
      (if (= cnt n)
          acc
          (loop (+ cnt 1) (chunked-string-append acc t)))))

  (define (unison-FOp-Text.reverse s)
    (chunked-string-foldMap-chunks
     s
     string-reverse
     (lambda (acc c) (chunked-string-append c acc))))

  (define (unison-FOp-Text.toLowercase s)
    (chunked-string-foldMap-chunks s string-downcase chunked-string-append))

  (define (unison-FOp-Text.toUppercase s)
    (chunked-string-foldMap-chunks s string-upcase chunked-string-append))

  (define (unison-FOp-Pattern.run p s)
    (let* ([r (pattern-match p s)])
      (if r (sum 1 (cdr r) (car r)) (sum 0))))

  (define (unison-FOp-Pattern.isMatch p s) (bool (pattern-match? p s)))
  (define (unison-FOp-Pattern.many p) (many p))
  (define (unison-FOp-Pattern.capture p) (capture p))
  (define (unison-FOp-Pattern.join ps)
    (join* ps))
  (define (unison-FOp-Pattern.or p1 p2) (choice p1 p2))
  (define (unison-FOp-Pattern.replicate n m p) (replicate p n m))

  (define (unison-FOp-Text.patterns.digit) digit)
  (define (unison-FOp-Text.patterns.letter) letter)
  (define (unison-FOp-Text.patterns.punctuation) punctuation)
  (define (unison-FOp-Text.patterns.charIn cs) (chars cs))
  (define (unison-FOp-Text.patterns.notCharIn cs) (not-chars cs))
  (define (unison-FOp-Text.patterns.anyChar) any-char)
  (define (unison-FOp-Text.patterns.space) space)
  (define (unison-FOp-Text.patterns.charRange a z) (char-range (integer->char a) (integer->char z)))
  (define (unison-FOp-Text.patterns.notCharRange a z) (not-char-range (integer->char a) (integer->char z)))
  (define (unison-FOp-Text.patterns.literal s) (literal s))
  (define (unison-FOp-Text.patterns.eof) eof)
  (define (unison-FOp-Text.patterns.char cc) cc)
  (define (unison-FOp-Char.Class.is cc c)
    (unison-FOp-Pattern.isMatch cc (unison-FOp-Char.toText c)))
  (define (unison-FOp-Char.Class.any) (unison-FOp-Text.patterns.anyChar))
  (define (unison-FOp-Char.Class.punctuation)
    (unison-FOp-Text.patterns.punctuation))
  (define (unison-FOp-Char.Class.letter) (unison-FOp-Text.patterns.letter))
  (define (unison-FOp-Char.Class.alphanumeric) alphanumeric)
  (define (unison-FOp-Char.Class.upper) upper)
  (define (unison-FOp-Char.Class.lower) lower)
  (define (unison-FOp-Char.Class.number) number)
  (define (unison-FOp-Char.Class.symbol) symbol)
  (define (unison-FOp-Char.Class.whitespace) space)
  (define (unison-FOp-Char.Class.control) control)
  (define (unison-FOp-Char.Class.printable) printable)
  (define (unison-FOp-Char.Class.mark) mark)
  (define (unison-FOp-Char.Class.separator) separator)
  (define (unison-FOp-Char.Class.or p1 p2) (char-class-or p1 p2))
  (define (unison-FOp-Char.Class.range a z)
    (unison-FOp-Text.patterns.charRange a z))
  (define (unison-FOp-Char.Class.anyOf cs) (unison-FOp-Text.patterns.charIn cs))
  (define (unison-FOp-Char.Class.and cc1 cc2) (char-class-and cc1 cc2))
  (define (unison-FOp-Char.Class.not cc) (char-class-not cc))

  (define (catch-array thunk)
    (reify-exn thunk))

  (define (unison-FOp-ImmutableArray.read vec i)
    (catch-array
      (lambda ()
        (sum 1 (vector-ref vec i)))))

  (define (unison-FOp-ImmutableArray.copyTo! dst doff src soff n)
    (catch-array
      (lambda ()
        (vector-copy! dst doff src soff n)
        (sum 1))))

  (define (unison-FOp-MutableArray.copyTo! dst doff src soff l)
    (catch-array
      (lambda ()
        (vector-copy! dst doff src soff l)
        (sum 1))))

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
        (bytes-copy! dst doff src soff n)
        (sum 1))))

  (define (unison-FOp-ImmutableByteArray.read8 arr i)
    (catch-array
      (lambda ()
        (sum 1 (bytevector-u8-ref arr i)))))

  (define (unison-FOp-MutableByteArray.copyTo! dst doff src soff l)
    (catch-array
      (lambda ()
        (bytes-copy! dst doff src soff l)
        (sum 1))))

  (define unison-FOp-MutableByteArray.freeze! freeze-bytevector!)

  (define (unison-FOp-MutableByteArray.write8 arr i b)
    (catch-array
      (lambda ()
        (bytevector-u8-set! arr i b)
        (sum 1))))

  (define (unison-FOp-MutableByteArray.write16be arr i b)
    (catch-array
      (lambda ()
        (bytevector-u16-set! arr i b 'big)
        (sum 1))))

  (define (unison-FOp-MutableByteArray.write32be arr i b)
    (catch-array
      (lambda ()
        (bytevector-u32-set! arr i b 'big)
        (sum 1))))

  (define (unison-FOp-MutableByteArray.write64be arr i b)
    (catch-array
      (lambda ()
        (bytevector-u64-set! arr i b 'big)
        (sum 1))))

  (define (unison-FOp-MutableByteArray.read8 arr i)
    (catch-array
      (lambda ()
        (sum 1 (bytevector-u8-ref arr i)))))

  (define (unison-FOp-MutableByteArray.read16be arr i)
    (catch-array
      (lambda ()
        (sum 1 (bytevector-u16-ref arr i 'big)))))

  (define (unison-FOp-MutableByteArray.read24be arr i)
    (catch-array
      (lambda ()
        (sum 1 (bytevector-u24-ref arr i 'big)))))

  (define (unison-FOp-MutableByteArray.read32be arr i)
    (catch-array
      (lambda ()
        (sum 1 (bytevector-u32-ref arr i 'big)))))

  (define (unison-FOp-MutableByteArray.read40be arr i)
    (catch-array
      (lambda ()
        (sum 1 (bytevector-u40-ref arr i 'big)))))

  (define (unison-FOp-MutableByteArray.read64be arr i)
    (catch-array
      (lambda ()
        (sum 1 (bytevector-u64-ref arr i 'big)))))

  (define (unison-FOp-Scope.bytearray n) (make-bytes n))
  (define (unison-FOp-IO.bytearray n) (make-bytes n))

  (define (unison-FOp-Scope.array n) (make-vector n))
  (define (unison-FOp-IO.array n) (make-vector n))

  (define (unison-FOp-Scope.bytearrayOf b n) (make-bytes n b))
  (define (unison-FOp-IO.bytearrayOf b n) (make-bytes n b))

  (define (unison-FOp-Scope.arrayOf v n) (make-vector n v))
  (define (unison-FOp-IO.arrayOf v n) (make-vector n v))

  (define unison-FOp-MutableByteArray.length bytevector-length)
  (define unison-FOp-ImmutableByteArray.length bytevector-length)
  (define unison-FOp-MutableByteArray.size bytevector-length)
  (define unison-FOp-ImmutableByteArray.size bytevector-length)
  (define unison-FOp-MutableArray.size vector-length)
  (define unison-FOp-ImmutableArray.size vector-length)

  (define (unison-POp-FORK thunk) (fork thunk))
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


(define (exn:io? e)
  (or (exn:fail:read? e)
      (exn:fail:filesystem? e)
      (exn:fail:network? e)))

(define (exn:arith? e)
  (or (exn:fail:contract:divide-by-zero? e)
      (exn:fail:contract:non-fixnum-result? e)))

(define-unison-builtin (builtin-IO.tryEval thunk)
  (with-handlers
    ([exn:break?
      (lambda (e)
        (raise-unison-exception
          ref-threadkilledfailure:typelink
          (string->chunked-string "thread killed")
          ref-unit-unit))]
     [exn:io?
       (lambda (e)
         (raise-unison-exception
           ref-iofailure:typelink
           (exception->string e)
           ref-unit-unit))]
     [exn:arith?
       (lambda (e)
         (raise-unison-exception
           ref-arithfailure:typelink
           (exception->string e)
           ref-unit-unit))]
     [exn:bug? (lambda (e) (exn:bug->exception e))]
     [exn:fail?
       (lambda (e)
         (raise-unison-exception
           ref-runtimefailure:typelink
           (exception->string e)
           ref-unit-unit))]
     [(lambda (x) #t)
      (lambda (e)
        (raise-unison-exception
          ref-miscfailure:typelink
          (exception->string e)
          ref-unit-unit))])
    (thunk ref-unit-unit)))

; (declare-builtin-link builtin-Float.*)
; (declare-builtin-link builtin-Float.fromRepresentation)
; (declare-builtin-link builtin-Float.toRepresentation)
; (declare-builtin-link builtin-Float.ceiling)
; (declare-builtin-link builtin-Float.exp)
; (declare-builtin-link builtin-Float.log)
; (declare-builtin-link builtin-Float.max)
; (declare-builtin-link builtin-Float.min)
; (declare-builtin-link builtin-Float.tan)
; (declare-builtin-link builtin-Float.tanh)
; (declare-builtin-link builtin-Float.logBase)
; (declare-builtin-link builtin-Float.pow)
; (declare-builtin-link builtin-Float.>)
; (declare-builtin-link builtin-Float.<)
; (declare-builtin-link builtin-Float.>=)
; (declare-builtin-link builtin-Float.<=)
; (declare-builtin-link builtin-Float.==)
; (declare-builtin-link builtin-Int.pow)
; (declare-builtin-link builtin-Int.*)
; (declare-builtin-link builtin-Int.+)
; (declare-builtin-link builtin-Int.-)
; (declare-builtin-link builtin-Int./)
; (declare-builtin-link builtin-Int.>)
; (declare-builtin-link builtin-Int.<)
; (declare-builtin-link builtin-Int.>=)
; (declare-builtin-link builtin-Int.<=)
; (declare-builtin-link builtin-Int.==)
; (declare-builtin-link builtin-Int.isEven)
; (declare-builtin-link builtin-Int.isOdd)
; (declare-builtin-link builtin-Int.increment)
; (declare-builtin-link builtin-Int.negate)
; (declare-builtin-link builtin-Int.fromRepresentation)
; (declare-builtin-link builtin-Int.toRepresentation)
; (declare-builtin-link builtin-Int.signum)
; (declare-builtin-link builtin-Int.trailingZeros)
; (declare-builtin-link builtin-Int.popCount)
; (declare-builtin-link builtin-Nat.increment)
; (declare-builtin-link builtin-Nat.popCount)
; (declare-builtin-link builtin-Nat.toFloat)
; (declare-builtin-link builtin-Nat.trailingZeros)
; (declare-builtin-link builtin-Nat.+)
; (declare-builtin-link builtin-Nat.>)
; (declare-builtin-link builtin-Nat.<)
; (declare-builtin-link builtin-Nat.>=)
; (declare-builtin-link builtin-Nat.<=)
; (declare-builtin-link builtin-Nat.==)
; (declare-builtin-link builtin-Nat.drop)
; (declare-builtin-link builtin-Nat.isEven)
; (declare-builtin-link builtin-Nat.isOdd)
; (declare-builtin-link builtin-Text.indexOf)
; (declare-builtin-link builtin-Text.>)
; (declare-builtin-link builtin-Text.<)
; (declare-builtin-link builtin-Text.>=)
; (declare-builtin-link builtin-Text.<=)
; (declare-builtin-link builtin-Text.==)
; (declare-builtin-link builtin-Text.!=)
; (declare-builtin-link builtin-Bytes.indexOf)
; (declare-builtin-link builtin-IO.randomBytes)
; (declare-builtin-link builtin-IO.tryEval)
; (declare-builtin-link builtin-List.splitLeft)
; (declare-builtin-link builtin-List.splitRight)
; (declare-builtin-link builtin-Value.toBuiltin)
; (declare-builtin-link builtin-Value.fromBuiltin)
; (declare-builtin-link builtin-Code.fromGroup)
; (declare-builtin-link builtin-Code.toGroup)
; (declare-builtin-link builtin-TermLink.fromReferent)
; (declare-builtin-link builtin-TermLink.toReferent)
; (declare-builtin-link builtin-TypeLink.toReference)
; (declare-builtin-link builtin-IO.seekHandle.impl.v3)
; (declare-builtin-link builtin-IO.getLine.impl.v1)
; (declare-builtin-link builtin-IO.getSomeBytes.impl.v1)
; (declare-builtin-link builtin-IO.setBuffering.impl.v3)
; (declare-builtin-link builtin-IO.getBuffering.impl.v3)
; (declare-builtin-link builtin-IO.setEcho.impl.v1)
; (declare-builtin-link builtin-IO.isFileOpen.impl.v3)
; (declare-builtin-link builtin-IO.ready.impl.v1)
; (declare-builtin-link builtin-IO.process.call)
; (declare-builtin-link builtin-IO.getEcho.impl.v1)
; (declare-builtin-link builtin-IO.getArgs.impl.v1)
; (declare-builtin-link builtin-IO.getEnv.impl.v1)
; (declare-builtin-link builtin-IO.getChar.impl.v1)
; (declare-builtin-link builtin-IO.directoryContents.impl.v3)
; (declare-builtin-link builtin-IO.getCurrentDirectory.impl.v3)
; (declare-builtin-link builtin-IO.removeDirectory.impl.v3)
; (declare-builtin-link builtin-IO.renameFile.impl.v3)
; (declare-builtin-link builtin-IO.createTempDirectory.impl.v3)
; (declare-builtin-link builtin-IO.createDirectory.impl.v3)
; (declare-builtin-link builtin-IO.setCurrentDirectory.impl.v3)
; (declare-builtin-link builtin-IO.renameDirectory.impl.v3)
; (declare-builtin-link builtin-IO.fileExists.impl.v3)
; (declare-builtin-link builtin-IO.isDirectory.impl.v3)
; (declare-builtin-link builtin-IO.isFileEOF.impl.v3)
; (declare-builtin-link builtin-IO.isSeekable.impl.v3)
; (declare-builtin-link builtin-IO.handlePosition.impl.v3)
; (declare-builtin-link builtin-IO.systemTime.impl.v3)
; (declare-builtin-link builtin-IO.systemTimeMicroseconds.impl.v3)
; (declare-builtin-link builtin-Universal.==)
; (declare-builtin-link builtin-Universal.>)
; (declare-builtin-link builtin-Universal.<)
; (declare-builtin-link builtin-Universal.>=)
; (declare-builtin-link builtin-Universal.<=)
; (declare-builtin-link builtin-Universal.compare)
; (declare-builtin-link builtin-Pattern.isMatch)
; (declare-builtin-link builtin-Scope.bytearrayOf)
; (declare-builtin-link builtin-Char.Class.is)
; (declare-builtin-link builtin-Pattern.many.corrected)
; (declare-builtin-link builtin-unsafe.coerceAbilities)
; (declare-builtin-link builtin-Clock.internals.systemTimeZone.v1)
