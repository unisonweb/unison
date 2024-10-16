; This library re-exports all of the builtin operation modules.
; Builtins are now directly implemented, rather than using the
; implementation details of the Haskell interpreter. The individual
; modules are divided to be somewhat more organized, but downstream
; modules can just require this one to get them all.
#lang racket/base

(provide
  (all-from-out
    unison/primops/array
    unison/primops/bytes
    unison/primops/concurrent
    unison/primops/crypto
    unison/primops/io
    unison/primops/io-handles
    unison/primops/list
    unison/primops/math
    unison/primops/misc
    unison/primops/pattern
    unison/primops/ref
    unison/primops/tcp
    unison/primops/text
    unison/primops/tls
    unison/primops/udp
    unison/primops/universal))

(require
  unison/primops/array
  unison/primops/bytes
  unison/primops/concurrent
  unison/primops/crypto
  unison/primops/io
  unison/primops/io-handles
  unison/primops/list
  unison/primops/math
  unison/primops/misc
  unison/primops/pattern
  unison/primops/ref
  unison/primops/tcp
  unison/primops/text
  unison/primops/tls
  unison/primops/udp
  unison/primops/universal)

