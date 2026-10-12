# Builtins with native implementations

Which of the builtin functions declared in `Unison.Builtin` (`parser-typechecker/src/Unison/Builtin.hs`)
the JIT runs natively, as of 2026-10-05. Checked means: the builtin's runtime definition in
`Unison.Runtime.Builtin` lowers to MCode instructions the code generator has native code for
(`instrNative` in `JIT/Codegen.hs`), or it is a foreign function the generator has a C version of,
so a call to it from compiled code never goes back to the interpreter. Unchecked entries say
what stands in the way: the primitive op or the foreign function (`foreign Code_serialize`) that
has no native version yet, or "no runtime implementation found" for the few names that have a
type in `Unison.Builtin` but no definition in the runtime at all.

This was produced by a static reading of the runtime's definitions against the generator's
lists, not by running each builtin, so a native entry means "every instruction in its body is
one the generator handles"; the generator may still take its slow path on an unusual argument
(a `Text.take` of something that isn't a text, say), which is by design. Things worth knowing
when reading it:

- All of Int, Nat and Float is native (since 2026-10-04: `pow`, `toFloat`, the `CAST`
  coercions behind `Nat.toInt`, `Char.toNat`, `Float.toRepresentation` and the like, and every
  Float operation). The Float results are the interpreter's bit for bit; the internals ("Numbers") say
  what that took.
- `Text`, `List` and `Universal` comparison are complete. `Bytes` is complete except the
  compression functions. The Text patterns were skipped on purpose (Paul, 2026-10-03).
- Arrays and Refs are complete (2026-10-05): every `MutableArray`, `ImmutableArray`,
  `MutableByteArray`, `ImmutableByteArray` and `PinnedByteArray` builtin, the `Scope` and `IO`
  constructors for them, `Scope.ref`/`IO.ref`, `Ref.readForCas`, `Ticket.read` and `Ref.cas`.
  See the internals, "Arrays and refs".
- `Universal.murmurHashUntyped` is native (2026-10-04) for numbers, characters, data
  constructors, text, bytes, lists, arrays and byte arrays; a function, a map, a link, quoted
  code, a continuation or a big number in the value sends the whole hash to the interpreter.
  The typed `Universal.murmurHash` serializes the value first and stays a call-out.
- Everything with effects (`IO`, `MVar`, `STM`, `Promise`, `Tls`, `Clock`, the `Code`/`Value`
  reflection, hashing and crypto, the FFI, the big-number `Integer`/`Natural`) is a call-out.
  Most of these are wrappers around Haskell libraries, and their cost is in the library, not
  the call; the ones a program is likely to call in a hot loop are the array reads and writes
  (`MutableByteArray.read8` and the like), `Ref.cas`, `Scope.ref`, the hashes, and the
  big-number arithmetic. (The array reads and writes, `Ref.cas`, `Scope.ref` and the untyped
  hash are native since 2026-10-05.)
- The `FFI.Ptr` reads and writes (`get`, `getAt`, `set`, `setAt` of every element type,
  including pointers) are native since 2026-10-11: a load or store through the address in the
  `Ptr`'s wrapper, with the conventions' sign and zero extension and truncation. Allocation,
  freeing, `cast`, `null`, the `ForeignPtr`s and the calls themselves (`getDLLSym`,
  `FFI.base`) stay call-outs. See the internals, "Arrays and refs".
- The five bare names `==`, `<`, `<=`, `>`, `>=` in `Unison.Builtin` are a rename list
  (`Int.==` is also `Int.eq`, and so on), not builtins, and are left out.
- The Haskell replacements of base definitions (`Map`, `Set`, `List.sort`, the JSON parser,
  ... in `functionReplacementList`) are not builtins and are not listed here. Under the JIT
  only the ones that beat native code are applied; which, and the numbers, are in the
  internals ("The Haskell replacements") and
  [benchmarks/2026-10-11-replacements.md](benchmarks/2026-10-11-replacements.md).

To regenerate after adding native versions: the lists this reads are `prim1Supported`,
`prim2Supported` and the `elem` lists in `instrNative`, `textForeign` and `bytesForeign` in
`Codegen.hs`, and the definitions in `Unison.Runtime.Builtin`.

## Summary

| Category | Native | Total |
| --- | --- | --- |
| Int | 31 | 31 |
| Nat | 28 | 28 |
| Float | 38 | 38 |
| Boolean | 1 | 1 |
| Char | 3 | 3 |
| Text | 21 | 21 |
| Text patterns | 0 | 44 |
| Bytes | 30 | 36 |
| List | 7 | 7 |
| Universal | 7 | 8 |
| Any, Debug and errors | 3 | 8 |
| Ref and Scope | 13 | 13 |
| Arrays | 48 | 48 |
| Code, Value and Link | 0 | 18 |
| Hashing and crypto | 0 | 13 |
| MVar, STM and Promise | 0 | 22 |
| IO | 7 | 70 |
| IO (UDP) | 0 | 11 |
| Tls | 0 | 25 |
| Clock | 0 | 7 |
| Sandboxing | 0 | 2 |
| Integer and Natural | 0 | 55 |
| FFI | 45 | 93 |
| **All** | **282** | **602** |

## Int (31 of 31)

- [x] `Int.+`
- [x] `Int.-`
- [x] `Int.*`
- [x] `Int./`
- [x] `Int.<`
- [x] `Int.>`
- [x] `Int.<=`
- [x] `Int.>=`
- [x] `Int.==`
- [x] `Int.and`
- [x] `Int.or`
- [x] `Int.xor`
- [x] `Int.complement`
- [x] `Int.increment`
- [x] `Int.isEven`
- [x] `Int.isOdd`
- [x] `Int.signum`
- [x] `Int.leadingZeros`
- [x] `Int.negate`
- [x] `Int.mod`
- [x] `Int.pow`
- [x] `Int.shiftLeft`
- [x] `Int.shiftRight`
- [x] `Int.truncate0`
- [x] `Int.toText`
- [x] `Int.fromText`
- [x] `Int.toFloat`
- [x] `Int.trailingZeros`
- [x] `Int.popCount`
- [x] `Int.fromRepresentation`
- [x] `Int.toRepresentation`

## Nat (28 of 28)

- [x] `Nat.*`
- [x] `Nat.+`
- [x] `Nat./`
- [x] `Nat.<`
- [x] `Nat.<=`
- [x] `Nat.==`
- [x] `Nat.>`
- [x] `Nat.>=`
- [x] `Nat.and`
- [x] `Nat.or`
- [x] `Nat.xor`
- [x] `Nat.complement`
- [x] `Nat.drop`
- [x] `Nat.fromText`
- [x] `Nat.increment`
- [x] `Nat.isEven`
- [x] `Nat.isOdd`
- [x] `Nat.leadingZeros`
- [x] `Nat.mod`
- [x] `Nat.pow`
- [x] `Nat.shiftLeft`
- [x] `Nat.shiftRight`
- [x] `Nat.sub`
- [x] `Nat.toFloat`
- [x] `Nat.toInt`
- [x] `Nat.toText`
- [x] `Nat.trailingZeros`
- [x] `Nat.popCount`

## Float (38 of 38)

- [x] `Float.+`
- [x] `Float.-`
- [x] `Float.*`
- [x] `Float./`
- [x] `Float.<`
- [x] `Float.>`
- [x] `Float.<=`
- [x] `Float.>=`
- [x] `Float.==`
- [x] `Float.fromRepresentation`
- [x] `Float.toRepresentation`
- [x] `Float.acos`
- [x] `Float.asin`
- [x] `Float.atan`
- [x] `Float.atan2`
- [x] `Float.cos`
- [x] `Float.sin`
- [x] `Float.tan`
- [x] `Float.acosh`
- [x] `Float.asinh`
- [x] `Float.atanh`
- [x] `Float.cosh`
- [x] `Float.sinh`
- [x] `Float.tanh`
- [x] `Float.exp`
- [x] `Float.log`
- [x] `Float.logBase`
- [x] `Float.pow`
- [x] `Float.sqrt`
- [x] `Float.ceiling`
- [x] `Float.floor`
- [x] `Float.round`
- [x] `Float.truncate`
- [x] `Float.abs`
- [x] `Float.max`
- [x] `Float.min`
- [x] `Float.toText`
- [x] `Float.fromText`

## Boolean (1 of 1)

- [x] `Boolean.not`

## Char (3 of 3)

- [x] `Char.toNat`
- [x] `Char.toText`
- [x] `Char.fromNat`

## Text (21 of 21)

- [x] `Text.empty`
- [x] `Text.++`
- [x] `Text.take`
- [x] `Text.drop`
- [x] `Text.indexOf`
- [x] `Text.size`
- [x] `Text.repeat`
- [x] `Text.==`
- [x] `Text.<=`
- [x] `Text.>=`
- [x] `Text.<`
- [x] `Text.>`
- [x] `Text.uncons`
- [x] `Text.unsnoc`
- [x] `Text.toCharList`
- [x] `Text.fromCharList`
- [x] `Text.reverse`
- [x] `Text.toUppercase`
- [x] `Text.toLowercase`
- [x] `Text.toUtf8`
- [x] `Text.fromUtf8.impl.v3`

## Text patterns (0 of 44)

- [ ] `Text.patterns.eof` (foreign Text_patterns_eof)
- [ ] `Text.patterns.anyChar` (foreign Text_patterns_anyChar)
- [ ] `Text.patterns.literal` (foreign Text_patterns_literal)
- [ ] `Text.patterns.digit` (foreign Text_patterns_digit)
- [ ] `Text.patterns.letter` (foreign Text_patterns_letter)
- [ ] `Text.patterns.space` (foreign Text_patterns_space)
- [ ] `Text.patterns.punctuation` (foreign Text_patterns_punctuation)
- [ ] `Text.patterns.charRange` (foreign Text_patterns_charRange)
- [ ] `Text.patterns.notCharRange` (foreign Text_patterns_notCharRange)
- [ ] `Text.patterns.charIn` (foreign Text_patterns_charIn)
- [ ] `Text.patterns.notCharIn` (foreign Text_patterns_notCharIn)
- [ ] `Text.patterns.lookbehind` (foreign Text_patterns_lookbehind1)
- [ ] `Text.patterns.negativeLookbehind` (foreign Text_patterns_negativeLookbehind1)
- [ ] `Pattern.many` (foreign Pattern_many)
- [ ] `Pattern.many.corrected` (foreign Pattern_many_corrected)
- [ ] `Pattern.replicate` (foreign Pattern_replicate)
- [ ] `Pattern.capture` (foreign Pattern_capture)
- [ ] `Pattern.captureAs` (foreign Pattern_captureAs)
- [ ] `Pattern.join` (foreign Pattern_join)
- [ ] `Pattern.or` (foreign Pattern_or)
- [ ] `Pattern.lookahead` (foreign Pattern_lookahead)
- [ ] `Pattern.negativeLookahead` (foreign Pattern_negativeLookahead)
- [ ] `Pattern.run` (foreign Pattern_run)
- [ ] `Pattern.isMatch` (foreign Pattern_isMatch)
- [ ] `Char.Class.any` (foreign Char_Class_any)
- [ ] `Char.Class.not` (foreign Char_Class_not)
- [ ] `Char.Class.and` (foreign Char_Class_and)
- [ ] `Char.Class.or` (foreign Char_Class_or)
- [ ] `Char.Class.range` (foreign Char_Class_range)
- [ ] `Char.Class.anyOf` (foreign Char_Class_anyOf)
- [ ] `Char.Class.alphanumeric` (foreign Char_Class_alphanumeric)
- [ ] `Char.Class.upper` (foreign Char_Class_upper)
- [ ] `Char.Class.lower` (foreign Char_Class_lower)
- [ ] `Char.Class.whitespace` (foreign Char_Class_whitespace)
- [ ] `Char.Class.control` (foreign Char_Class_control)
- [ ] `Char.Class.printable` (foreign Char_Class_printable)
- [ ] `Char.Class.mark` (foreign Char_Class_mark)
- [ ] `Char.Class.number` (foreign Char_Class_number)
- [ ] `Char.Class.punctuation` (foreign Char_Class_punctuation)
- [ ] `Char.Class.symbol` (foreign Char_Class_symbol)
- [ ] `Char.Class.separator` (foreign Char_Class_separator)
- [ ] `Char.Class.letter` (foreign Char_Class_letter)
- [ ] `Char.Class.is` (foreign Char_Class_is)
- [ ] `Text.patterns.char` (foreign Text_patterns_char)

## Bytes (30 of 36)

- [x] `Bytes.decodeNat64be`
- [x] `Bytes.decodeNat64le`
- [x] `Bytes.decodeNat32be`
- [x] `Bytes.decodeNat32le`
- [x] `Bytes.decodeNat16be`
- [x] `Bytes.decodeNat16le`
- [x] `Bytes.encodeNat64be`
- [x] `Bytes.encodeNat64le`
- [x] `Bytes.encodeNat32be`
- [x] `Bytes.encodeNat32le`
- [x] `Bytes.encodeNat16be`
- [x] `Bytes.encodeNat16le`
- [x] `Bytes.empty`
- [x] `Bytes.fromList`
- [x] `Bytes.++`
- [x] `Bytes.take`
- [x] `Bytes.drop`
- [x] `Bytes.at`
- [x] `Bytes.indexOf`
- [x] `Bytes.toList`
- [x] `Bytes.size`
- [x] `Bytes.flatten`
- [ ] `Bytes.zlib.compress` (foreign Bytes_zlib_compress)
- [ ] `Bytes.zlib.decompress` (foreign Bytes_zlib_decompress)
- [ ] `Bytes.gzip.compress` (foreign Bytes_gzip_compress)
- [ ] `Bytes.gzip.decompress` (foreign Bytes_gzip_decompress)
- [ ] `Bytes.zstd.compress` (foreign Bytes_zstd_compress)
- [ ] `Bytes.zstd.decompress` (foreign Bytes_zstd_decompress)
- [x] `Bytes.toBase16`
- [x] `Bytes.toBase32`
- [x] `Bytes.toBase64`
- [x] `Bytes.toBase64UrlUnpadded`
- [x] `Bytes.fromBase16`
- [x] `Bytes.fromBase32`
- [x] `Bytes.fromBase64`
- [x] `Bytes.fromBase64UrlUnpadded`

## List (7 of 7)

- [x] `List.cons`
- [x] `List.snoc`
- [x] `List.take`
- [x] `List.drop`
- [x] `List.++`
- [x] `List.size`
- [x] `List.at`

## Universal (7 of 8)

- [x] `Universal.==`
- [x] `Universal.compare`
- [x] `Universal.>`
- [x] `Universal.<`
- [x] `Universal.>=`
- [x] `Universal.<=`
- [ ] `Universal.murmurHash` (foreign Universal_murmurHash)
- [x] `Universal.murmurHashUntyped`

## Any, Debug and errors (3 of 8)

- [x] `Any.unsafeExtract`
- [ ] `bug` (EROR)
- [ ] `todo` (EROR)
- [x] `Any.Any`
- [ ] `Debug.watch` (PRNT, TPrm PRNT)
- [ ] `Debug.trace` (TRCE)
- [ ] `Debug.toText` (DBTX)
- [x] `unsafe.coerceAbilities`

## Ref and Scope (13 of 13)

- [x] `Scope.run`
- [x] `Scope.ref`
- [x] `Ref.read`
- [x] `Ref.write`
- [x] `Scope.array`
- [x] `Scope.arrayOf`
- [x] `Scope.bytearray`
- [x] `Scope.bytearrayOf`
- [x] `Scope.pinnedByteArray`
- [x] `Scope.pinnedByteArrayOf`
- [x] `Ref.Ticket.read`
- [x] `Ref.readForCas`
- [x] `Ref.cas`

## Arrays (48 of 48)

- [x] `MutableArray.size`
- [x] `MutableByteArray.size`
- [x] `ImmutableArray.size`
- [x] `ImmutableByteArray.size`
- [x] `MutableArray.copyTo!`
- [x] `MutableByteArray.copyTo!`
- [x] `MutableArray.read`
- [x] `MutableByteArray.read8`
- [x] `MutableByteArray.read16be`
- [x] `MutableByteArray.read24be`
- [x] `MutableByteArray.read32be`
- [x] `MutableByteArray.read40be`
- [x] `MutableByteArray.read64be`
- [x] `MutableByteArray.read16le`
- [x] `MutableByteArray.read24le`
- [x] `MutableByteArray.read32le`
- [x] `MutableByteArray.read40le`
- [x] `MutableByteArray.read64le`
- [x] `MutableArray.write`
- [x] `MutableByteArray.write8`
- [x] `MutableByteArray.write16be`
- [x] `MutableByteArray.write32be`
- [x] `MutableByteArray.write64be`
- [x] `MutableByteArray.write16le`
- [x] `MutableByteArray.write32le`
- [x] `MutableByteArray.write64le`
- [x] `ImmutableArray.copyTo!`
- [x] `ImmutableByteArray.copyTo!`
- [x] `ImmutableArray.read`
- [x] `ImmutableByteArray.read8`
- [x] `ImmutableByteArray.read16be`
- [x] `ImmutableByteArray.read24be`
- [x] `ImmutableByteArray.read32be`
- [x] `ImmutableByteArray.read40be`
- [x] `ImmutableByteArray.read64be`
- [x] `ImmutableByteArray.read16le`
- [x] `ImmutableByteArray.read24le`
- [x] `ImmutableByteArray.read32le`
- [x] `ImmutableByteArray.read40le`
- [x] `ImmutableByteArray.read64le`
- [x] `MutableArray.freeze!`
- [x] `MutableByteArray.freeze!`
- [x] `MutableArray.freeze`
- [x] `MutableByteArray.freeze`
- [x] `ImmutableByteArray.toBytes`
- [x] `ImmutableByteArray.fromBytes`
- [x] `PinnedByteArray.cast`
- [x] `PinnedByteArray.contents`

## Code, Value and Link (0 of 18)

- [ ] `Value.validateSandboxed` (SDBV)
- [ ] `Code.dependencies` (foreign Code_dependencies)
- [ ] `Code.isMissing` (MISS)
- [ ] `Code.serialize` (foreign Code_serialize)
- [ ] `Code.serialize.versioned` (foreign Code_serialize_versioned)
- [ ] `Code.deserialize` (foreign Code_deserialize)
- [ ] `Code.cache_` (CACH)
- [ ] `Code.validate` (CVLD)
- [ ] `Code.lookup` (LKUP)
- [ ] `Code.display` (foreign Code_display)
- [ ] `Code.validateLinks` (foreign Code_validateLinks)
- [ ] `Value.dependencies` (foreign Value_dependencies)
- [ ] `Value.serialize` (foreign Value_serialize)
- [ ] `Value.serialize.versioned` (foreign Value_serialize_versioned)
- [ ] `Value.deserialize` (foreign Value_deserialize)
- [ ] `Value.value` (VALU)
- [ ] `Value.load` (LOAD)
- [ ] `Link.Term.toText` (TLTT)

## Hashing and crypto (0 of 13)

- [ ] `crypto.hash` (foreign Crypto_hash)
- [ ] `crypto.hashBytes` (foreign Crypto_hashBytes)
- [ ] `crypto.hmac` (foreign Crypto_hmac)
- [ ] `crypto.hmacBytes` (foreign Crypto_hmacBytes)
- [ ] `crypto.Ed25519.sign.impl` (foreign Crypto_Ed25519_sign_impl)
- [ ] `crypto.Ed25519.verify.impl` (foreign Crypto_Ed25519_verify_impl)
- [ ] `crypto.Rsa.sign.impl` (foreign Crypto_Rsa_sign_impl)
- [ ] `crypto.Rsa.verify.impl` (foreign Crypto_Rsa_verify_impl)
- [ ] `crypto.P256.publicKey.impl` (foreign Crypto_P256_publicKey_impl)
- [ ] `crypto.P256.signSha256.impl` (foreign Crypto_P256_signSha256_impl)
- [ ] `crypto.P256.verifySha256.impl` (foreign Crypto_P256_verifySha256_impl)
- [ ] `crypto.argon2.hashRaw` (foreign Crypto_Argon2_HashRaw)
- [ ] `crypto.argon2.verifyRaw` (foreign Crypto_Argon2_VerifyRaw)

## MVar, STM and Promise (0 of 22)

- [ ] `MVar.new` (foreign MVar_new)
- [ ] `MVar.newEmpty.v2` (foreign MVar_newEmpty_v2)
- [ ] `MVar.take.impl.v3` (foreign MVar_take_impl_v3)
- [ ] `MVar.tryTake` (foreign MVar_tryTake)
- [ ] `MVar.put.impl.v3` (foreign MVar_put_impl_v3)
- [ ] `MVar.tryPut.impl.v3` (foreign MVar_tryPut_impl_v3)
- [ ] `MVar.swap.impl.v3` (foreign MVar_swap_impl_v3)
- [ ] `MVar.isEmpty` (foreign MVar_isEmpty)
- [ ] `MVar.read.impl.v3` (foreign MVar_read_impl_v3)
- [ ] `MVar.tryRead.impl.v3` (foreign MVar_tryRead_impl_v3)
- [ ] `TVar.new` (foreign TVar_new)
- [ ] `TVar.newIO` (foreign TVar_newIO)
- [ ] `TVar.read` (foreign TVar_read)
- [ ] `TVar.readIO` (foreign TVar_readIO)
- [ ] `TVar.write` (foreign TVar_write)
- [ ] `TVar.swap` (foreign TVar_swap)
- [ ] `STM.retry` (foreign STM_retry)
- [ ] `STM.atomically` (ATOM, TPrm ATOM)
- [ ] `Promise.new` (foreign Promise_new)
- [ ] `Promise.read` (foreign Promise_read)
- [ ] `Promise.tryRead` (foreign Promise_tryRead)
- [ ] `Promise.write` (foreign Promise_write)

## IO (7 of 70)

- [ ] `Socket.toText` (foreign Socket_toText)
- [ ] `Handle.toText` (foreign Handle_toText)
- [ ] `ThreadId.toText` (foreign ThreadId_toText)
- [ ] `IO.keepAlive` (KEEP, TPrm KEEP)
- [ ] `IO.openFile.impl.v3` (foreign IO_openFile_impl_v3)
- [ ] `IO.closeFile.impl.v3` (foreign IO_closeFile_impl_v3)
- [ ] `IO.isFileEOF.impl.v3` (foreign IO_isFileEOF_impl_v3)
- [ ] `IO.isFileOpen.impl.v3` (foreign IO_isFileOpen_impl_v3)
- [ ] `IO.isSeekable.impl.v3` (foreign IO_isSeekable_impl_v3)
- [ ] `IO.seekHandle.impl.v3` (foreign IO_seekHandle_impl_v3)
- [ ] `IO.handlePosition.impl.v3` (foreign IO_handlePosition_impl_v3)
- [ ] `IO.getEnv.impl.v1` (foreign IO_getEnv_impl_v1)
- [ ] `IO.getArgs.impl.v1` (foreign IO_getArgs_impl_v1)
- [ ] `IO.getBuffering.impl.v3` (foreign IO_getBuffering_impl_v3)
- [ ] `IO.setBuffering.impl.v3` (foreign IO_setBuffering_impl_v3)
- [ ] `IO.getChar.impl.v1` (foreign IO_getChar_impl_v1)
- [ ] `IO.getEcho.impl.v1` (foreign IO_getEcho_impl_v1)
- [ ] `IO.ready.impl.v1` (foreign IO_ready_impl_v1)
- [ ] `IO.setEcho.impl.v1` (foreign IO_setEcho_impl_v1)
- [ ] `IO.getBytes.impl.v3` (foreign IO_getBytes_impl_v3)
- [ ] `IO.getSomeBytes.impl.v1` (foreign IO_getSomeBytes_impl_v1)
- [ ] `IO.putBytes.impl.v3` (foreign IO_putBytes_impl_v3)
- [ ] `IO.getLine.impl.v1` (foreign IO_getLine_impl_v1)
- [ ] `IO.fillBuf.impl.v1` (foreign IO_fillBuf_impl_v1)
- [ ] `IO.putBuf.impl.v1` (foreign IO_putBuf_impl_v1)
- [ ] `IO.getBufSome.impl.v1` (foreign IO_getBufSome_impl_v1)
- [ ] `IO.systemTime.impl.v3` (foreign IO_systemTime_impl_v3)
- [ ] `IO.systemTimeMicroseconds.v1` (foreign IO_systemTimeMicroseconds_v1)
- [ ] `IO.getTempDirectory.impl.v3` (foreign IO_getTempDirectory_impl_v3)
- [ ] `IO.createTempDirectory.impl.v3` (foreign IO_createTempDirectory_impl_v3)
- [ ] `IO.getCurrentDirectory.impl.v3` (foreign IO_getCurrentDirectory_impl_v3)
- [ ] `IO.setCurrentDirectory.impl.v3` (foreign IO_setCurrentDirectory_impl_v3)
- [ ] `IO.fileExists.impl.v3` (foreign IO_fileExists_impl_v3)
- [ ] `IO.isDirectory.impl.v3` (foreign IO_isDirectory_impl_v3)
- [ ] `IO.createDirectory.impl.v3` (foreign IO_createDirectory_impl_v3)
- [ ] `IO.removeDirectory.impl.v3` (foreign IO_removeDirectory_impl_v3)
- [ ] `IO.renameDirectory.impl.v3` (foreign IO_renameDirectory_impl_v3)
- [ ] `IO.directoryContents.impl.v3` (foreign IO_directoryContents_impl_v3)
- [ ] `IO.removeFile.impl.v3` (foreign IO_removeFile_impl_v3)
- [ ] `IO.renameFile.impl.v3` (foreign IO_renameFile_impl_v3)
- [ ] `IO.getFileTimestamp.impl.v3` (foreign IO_getFileTimestamp_impl_v3)
- [ ] `IO.getFileSize.impl.v3` (foreign IO_getFileSize_impl_v3)
- [ ] `IO.serverSocket.impl.v3` (foreign IO_serverSocket_impl_v3)
- [ ] `IO.listen.impl.v3` (foreign IO_listen_impl_v3)
- [ ] `IO.clientSocket.impl.v3` (foreign IO_clientSocket_impl_v3)
- [ ] `IO.closeSocket.impl.v3` (foreign IO_closeSocket_impl_v3)
- [ ] `IO.socketPort.impl.v3` (foreign IO_socketPort_impl_v3)
- [ ] `IO.socketAccept.impl.v3` (foreign IO_socketAccept_impl_v3)
- [ ] `IO.socketSend.impl.v3` (foreign IO_socketSend_impl_v3)
- [ ] `IO.socketReceive.impl.v3` (foreign IO_socketReceive_impl_v3)
- [ ] `IO.socketSendBuf.impl.v1` (foreign IO_socketSendBuf_impl_v1)
- [ ] `IO.socketReceiveBuf.impl.v1` (foreign IO_socketReceiveBuf_impl_v1)
- [ ] `IO.forkComp.v2` (FORK, TPrm FORK)
- [ ] `IO.stdHandle` (foreign IO_stdHandle)
- [ ] `IO.delay.impl.v3` (foreign IO_delay_impl_v3)
- [ ] `IO.kill.impl.v3` (foreign IO_kill_impl_v3)
- [x] `IO.ref`
- [ ] `IO.process.call` (foreign IO_process_call)
- [ ] `IO.process.start` (foreign IO_process_start)
- [ ] `IO.process.kill` (foreign IO_process_kill)
- [ ] `IO.process.wait` (foreign IO_process_wait)
- [ ] `IO.process.exitCode` (foreign IO_process_exitCode)
- [x] `IO.array`
- [x] `IO.arrayOf`
- [x] `IO.bytearray`
- [x] `IO.bytearrayOf`
- [x] `IO.pinnedByteArray`
- [x] `IO.pinnedByteArrayOf`
- [ ] `IO.tryEval` (TFRC, TPrm TFRC)
- [ ] `IO.randomBytes` (foreign IO_randomBytes)

## IO (UDP) (0 of 11)

- [ ] `IO.UDP.clientSocket.impl.v1` (foreign IO_UDP_clientSocket_impl_v1)
- [ ] `IO.UDP.ClientSockAddr.toText.v1` (foreign IO_UDP_ClientSockAddr_toText_v1)
- [ ] `IO.UDP.UDPSocket.toText.impl.v1` (foreign IO_UDP_UDPSocket_toText_impl_v1)
- [ ] `IO.UDP.UDPSocket.close.impl.v1` (foreign IO_UDP_UDPSocket_close_impl_v1)
- [ ] `IO.UDP.serverSocket.impl.v1` (foreign IO_UDP_serverSocket_impl_v1)
- [ ] `IO.UDP.ListenSocket.recvFrom.impl.v1` (foreign IO_UDP_ListenSocket_recvFrom_impl_v1)
- [ ] `IO.UDP.ListenSocket.sendTo.impl.v1` (foreign IO_UDP_ListenSocket_sendTo_impl_v1)
- [ ] `IO.UDP.ListenSocket.toText.impl.v1` (foreign IO_UDP_ListenSocket_toText_impl_v1)
- [ ] `IO.UDP.ListenSocket.close.impl.v1` (foreign IO_UDP_ListenSocket_close_impl_v1)
- [ ] `IO.UDP.UDPSocket.recv.impl.v1` (foreign IO_UDP_UDPSocket_recv_impl_v1)
- [ ] `IO.UDP.UDPSocket.send.impl.v1` (foreign IO_UDP_UDPSocket_send_impl_v1)

## Tls (0 of 25)

- [ ] `Tls.newClient.impl.v3` (foreign Tls_newClient_impl_v3)
- [ ] `Tls.newServer.impl.v3` (foreign Tls_newServer_impl_v3)
- [ ] `Tls.handshake.impl.v3` (foreign Tls_handshake_impl_v3)
- [ ] `Tls.send.impl.v3` (foreign Tls_send_impl_v3)
- [ ] `Tls.decodeCert.impl.v3` (foreign Tls_decodeCert_impl_v3)
- [ ] `Tls.encodeCert` (foreign Tls_encodeCert)
- [ ] `Tls.decodePrivateKey` (foreign Tls_decodePrivateKey)
- [ ] `Tls.encodePrivateKey` (foreign Tls_encodePrivateKey)
- [ ] `Tls.receive.impl.v3` (foreign Tls_receive_impl_v3)
- [ ] `Tls.terminate.impl.v3` (foreign Tls_terminate_impl_v3)
- [ ] `Tls.negotiatedProtocol` (foreign Tls_negotiatedProtocol)
- [ ] `Tls.ClientConfig.default` (foreign Tls_ClientConfig_default)
- [ ] `Tls.ServerConfig.default` (foreign Tls_ServerConfig_default)
- [ ] `TLS.ClientConfig.ciphers.set` (no runtime implementation found)
- [ ] `Tls.ServerConfig.ciphers.set` (no runtime implementation found)
- [ ] `Tls.ClientConfig.certificates.set` (foreign Tls_ClientConfig_certificates_set)
- [ ] `Tls.ClientConfig.certificates.get` (foreign Tls_ClientConfig_certificates_get)
- [ ] `Tls.ClientConfig.alpn.set` (foreign Tls_ClientConfig_alpn_set)
- [ ] `Tls.ServerConfig.alpn.set` (foreign Tls_ServerConfig_alpn_set)
- [ ] `Tls.ServerConfig.certificates.set` (foreign Tls_ServerConfig_certificates_set)
- [ ] `Tls.ServerConfig.certificates.get` (foreign Tls_ServerConfig_certificates_get)
- [ ] `Tls.ClientConfig.validation.disableHostNameValidation` (foreign Tls_ClientConfig_validation_disableHostNameValidation)
- [ ] `Tls.ClientConfig.validation.disableCertificateValidation` (foreign Tls_ClientConfig_validation_disableCertificateValidation)
- [ ] `Tls.ClientConfig.versions.set` (no runtime implementation found)
- [ ] `Tls.ServerConfig.versions.set` (no runtime implementation found)

## Clock (0 of 7)

- [ ] `Clock.internals.monotonic.v1` (foreign Clock_internals_monotonic_v1)
- [ ] `Clock.internals.processCPUTime.v1` (foreign Clock_internals_processCPUTime_v1)
- [ ] `Clock.internals.threadCPUTime.v1` (foreign Clock_internals_threadCPUTime_v1)
- [ ] `Clock.internals.realtime.v1` (foreign Clock_internals_realtime_v1)
- [ ] `Clock.internals.sec.v1` (foreign Clock_internals_sec_v1)
- [ ] `Clock.internals.nsec.v1` (foreign Clock_internals_nsec_v1)
- [ ] `Clock.internals.systemTimeZone.v1` (foreign Clock_internals_systemTimeZone_v1)

## Sandboxing (0 of 2)

- [ ] `validateSandboxed` (SDBX)
- [ ] `sandboxLinks` (SDBL)

## Integer and Natural (0 of 55)

- [ ] `Integer.fromText` (foreign Integer_fromText)
- [ ] `Natural.fromText` (foreign Natural_fromText)
- [ ] `Integer.unsafeFromText` (foreign Integer_unsafeFromText)
- [ ] `Natural.unsafeFromText` (foreign Natural_unsafeFromText)
- [ ] `Integer.toText` (foreign Integer_toText)
- [ ] `Natural.toText` (foreign Natural_toText)
- [ ] `Integer.fromInt` (foreign Integer_fromInt)
- [ ] `Natural.fromNat` (foreign Natural_fromNat)
- [ ] `Integer.toInt` (foreign Integer_toInt)
- [ ] `Natural.toNat` (foreign Natural_toNat)
- [ ] `Integer.add` (foreign Integer_add)
- [ ] `Integer.sub` (foreign Integer_sub)
- [ ] `Integer.mul` (foreign Integer_mul)
- [ ] `Integer.div` (foreign Integer_div)
- [ ] `Integer.mod` (foreign Integer_mod)
- [ ] `Integer.pow` (foreign Integer_pow)
- [ ] `Integer.shiftLeft` (foreign Integer_shl)
- [ ] `Integer.shiftRight` (foreign Integer_shr)
- [ ] `Integer.and` (foreign Integer_and)
- [ ] `Integer.or` (foreign Integer_or)
- [ ] `Integer.xor` (foreign Integer_xor)
- [ ] `Integer.not` (no runtime implementation found)
- [ ] `Integer.popCount` (foreign Integer_popCount)
- [ ] `Integer.eq` (foreign Integer_eq)
- [ ] `Integer.lt` (foreign Integer_lt)
- [ ] `Integer.lteq` (foreign Integer_le)
- [ ] `Integer.gt` (foreign Integer_gt)
- [ ] `Integer.gteq` (foreign Integer_ge)
- [ ] `Integer.neg` (foreign Integer_neg)
- [ ] `Integer.abs` (foreign Integer_abs)
- [ ] `Integer.signum` (foreign Integer_signum)
- [ ] `Integer.toFloat` (foreign Integer_toFloat)
- [ ] `Integer.isEven` (foreign Integer_isEven)
- [ ] `Integer.isOdd` (foreign Integer_isOdd)
- [ ] `Natural.add` (foreign Natural_add)
- [ ] `Natural.sub` (foreign Natural_sub)
- [ ] `Natural.mul` (foreign Natural_mul)
- [ ] `Natural.div` (foreign Natural_div)
- [ ] `Natural.mod` (foreign Natural_mod)
- [ ] `Natural.pow` (foreign Natural_pow)
- [ ] `Natural.shiftLeft` (foreign Natural_shl)
- [ ] `Natural.shiftRight` (foreign Natural_shr)
- [ ] `Natural.and` (foreign Natural_and)
- [ ] `Natural.or` (foreign Natural_or)
- [ ] `Natural.xor` (foreign Natural_xor)
- [ ] `Natural.not` (no runtime implementation found)
- [ ] `Natural.popCount` (foreign Natural_popCount)
- [ ] `Natural.eq` (foreign Natural_eq)
- [ ] `Natural.lt` (foreign Natural_lt)
- [ ] `Natural.lteq` (foreign Natural_le)
- [ ] `Natural.gt` (foreign Natural_gt)
- [ ] `Natural.gteq` (foreign Natural_ge)
- [ ] `Natural.toFloat` (foreign Natural_toFloat)
- [ ] `Natural.isEven` (foreign Natural_isEven)
- [ ] `Natural.isOdd` (foreign Natural_isOdd)

## FFI (45 of 93)

- [ ] `FFI.openDLL` (foreign FFI_openDLL)
- [ ] `FFI.int8` (foreign FFI_int8)
- [ ] `FFI.int16` (foreign FFI_int16)
- [ ] `FFI.int32` (foreign FFI_int32)
- [ ] `FFI.int64` (foreign FFI_int64)
- [ ] `FFI.uint64` (foreign FFI_uint64)
- [ ] `FFI.uint32` (foreign FFI_uint32)
- [ ] `FFI.uint16` (foreign FFI_uint16)
- [ ] `FFI.uint8` (foreign FFI_uint8)
- [ ] `FFI.double` (foreign FFI_double)
- [ ] `FFI.float` (foreign FFI_float)
- [ ] `FFI.void` (foreign FFI_void)
- [ ] `FFI.ptr` (foreign FFI_ptr)
- [ ] `FFI.pinnedByteArray` (foreign FFI_pinnedByteArray)
- [ ] `FFI.base` (foreign FFI_base)
- [ ] `FFI.baseIO` (foreign FFI_baseIO)
- [ ] `FFI.arr` (foreign FFI_arr)
- [ ] `FFI.getDLLSym` (foreign FFI_getDLLSym)
- [ ] `FFI.getDLLSymPtr` (foreign FFI_getDLLSymPtr)
- [ ] `FFI.Ptr.Int8.allocate` (foreign FFI_Ptr_Int8_allocate)
- [ ] `FFI.Ptr.Int16.allocate` (foreign FFI_Ptr_Int16_allocate)
- [ ] `FFI.Ptr.Int32.allocate` (foreign FFI_Ptr_Int32_allocate)
- [ ] `FFI.Ptr.Int.allocate` (foreign FFI_Ptr_Int_allocate)
- [ ] `FFI.Ptr.Nat8.allocate` (foreign FFI_Ptr_Nat8_allocate)
- [ ] `FFI.Ptr.Nat16.allocate` (foreign FFI_Ptr_Nat16_allocate)
- [ ] `FFI.Ptr.Nat32.allocate` (foreign FFI_Ptr_Nat32_allocate)
- [ ] `FFI.Ptr.Nat.allocate` (foreign FFI_Ptr_Nat_allocate)
- [ ] `FFI.Ptr.Float32.allocate` (foreign FFI_Ptr_Float32_allocate)
- [ ] `FFI.Ptr.Float.allocate` (foreign FFI_Ptr_Float_allocate)
- [x] `FFI.Ptr.Int8.get` (foreign FFI_Ptr_Int8_get)
- [x] `FFI.Ptr.Int16.get` (foreign FFI_Ptr_Int16_get)
- [x] `FFI.Ptr.Int32.get` (foreign FFI_Ptr_Int32_get)
- [x] `FFI.Ptr.Int.get` (foreign FFI_Ptr_Int_get)
- [x] `FFI.Ptr.Nat8.get` (foreign FFI_Ptr_Nat8_get)
- [x] `FFI.Ptr.Nat16.get` (foreign FFI_Ptr_Nat16_get)
- [x] `FFI.Ptr.Nat32.get` (foreign FFI_Ptr_Nat32_get)
- [x] `FFI.Ptr.Nat.get` (foreign FFI_Ptr_Nat_get)
- [x] `FFI.Ptr.Float32.get` (foreign FFI_Ptr_Float32_get)
- [x] `FFI.Ptr.Float.get` (foreign FFI_Ptr_Float_get)
- [x] `FFI.Ptr.Int8.getAt` (foreign FFI_Ptr_Int8_getAt)
- [x] `FFI.Ptr.Int16.getAt` (foreign FFI_Ptr_Int16_getAt)
- [x] `FFI.Ptr.Int32.getAt` (foreign FFI_Ptr_Int32_getAt)
- [x] `FFI.Ptr.Int.getAt` (foreign FFI_Ptr_Int_getAt)
- [x] `FFI.Ptr.Nat8.getAt` (foreign FFI_Ptr_Nat8_getAt)
- [x] `FFI.Ptr.Nat16.getAt` (foreign FFI_Ptr_Nat16_getAt)
- [x] `FFI.Ptr.Nat32.getAt` (foreign FFI_Ptr_Nat32_getAt)
- [x] `FFI.Ptr.Nat.getAt` (foreign FFI_Ptr_Nat_getAt)
- [x] `FFI.Ptr.Float32.getAt` (foreign FFI_Ptr_Float32_getAt)
- [x] `FFI.Ptr.Float.getAt` (foreign FFI_Ptr_Float_getAt)
- [x] `FFI.Ptr.Int8.set` (foreign FFI_Ptr_Int8_set)
- [x] `FFI.Ptr.Int16.set` (foreign FFI_Ptr_Int16_set)
- [x] `FFI.Ptr.Int32.set` (foreign FFI_Ptr_Int32_set)
- [x] `FFI.Ptr.Int.set` (foreign FFI_Ptr_Int_set)
- [x] `FFI.Ptr.Nat8.set` (foreign FFI_Ptr_Nat8_set)
- [x] `FFI.Ptr.Nat16.set` (foreign FFI_Ptr_Nat16_set)
- [x] `FFI.Ptr.Nat32.set` (foreign FFI_Ptr_Nat32_set)
- [x] `FFI.Ptr.Nat.set` (foreign FFI_Ptr_Nat_set)
- [x] `FFI.Ptr.Float32.set` (foreign FFI_Ptr_Float32_set)
- [x] `FFI.Ptr.Float.set` (foreign FFI_Ptr_Float_set)
- [x] `FFI.Ptr.Int8.setAt` (foreign FFI_Ptr_Int8_setAt)
- [x] `FFI.Ptr.Int16.setAt` (foreign FFI_Ptr_Int16_setAt)
- [x] `FFI.Ptr.Int32.setAt` (foreign FFI_Ptr_Int32_setAt)
- [x] `FFI.Ptr.Int.setAt` (foreign FFI_Ptr_Int_setAt)
- [x] `FFI.Ptr.Nat8.setAt` (foreign FFI_Ptr_Nat8_setAt)
- [x] `FFI.Ptr.Nat16.setAt` (foreign FFI_Ptr_Nat16_setAt)
- [x] `FFI.Ptr.Nat32.setAt` (foreign FFI_Ptr_Nat32_setAt)
- [x] `FFI.Ptr.Nat.setAt` (foreign FFI_Ptr_Nat_setAt)
- [x] `FFI.Ptr.Float32.setAt` (foreign FFI_Ptr_Float32_setAt)
- [x] `FFI.Ptr.Float.setAt` (foreign FFI_Ptr_Float_setAt)
- [ ] `FFI.Ptr.Ptr.allocate` (foreign FFI_Ptr_Ptr_allocate)
- [x] `FFI.Ptr.Ptr.get` (foreign FFI_Ptr_Ptr_get)
- [x] `FFI.Ptr.Ptr.set` (foreign FFI_Ptr_Ptr_set)
- [x] `FFI.Ptr.Ptr.getAt` (foreign FFI_Ptr_Ptr_getAt)
- [x] `FFI.Ptr.Ptr.setAt` (foreign FFI_Ptr_Ptr_setAt)
- [ ] `FFI.Ptr.free` (foreign FFI_Ptr_free)
- [x] `FFI.Ptr.cast`
- [ ] `FFI.Ptr.null` (foreign FFI_Ptr_null)
- [ ] `FFI.ForeignPtr.new.foreign` (foreign FFI_ForeignPtr_new_foreign)
- [ ] `FFI.ForeignPtr.new` (FGNN)
- [ ] `FFI.ForeignPtr.addCFinalizer` (foreign FFI_ForeignPtr_addCFinalizer)
- [ ] `FFI.ForeignPtr.addFinalizer` (FGNF)
- [ ] `FFI.ForeignPtr.unsafeContents` (foreign FFI_ForeignPtr_unsafeContents)
- [ ] `FFI.ForeignPtr.Int8.allocate` (foreign FFI_ForeignPtr_Int8_allocate)
- [ ] `FFI.ForeignPtr.Int16.allocate` (foreign FFI_ForeignPtr_Int16_allocate)
- [ ] `FFI.ForeignPtr.Int32.allocate` (foreign FFI_ForeignPtr_Int32_allocate)
- [ ] `FFI.ForeignPtr.Int.allocate` (foreign FFI_ForeignPtr_Int_allocate)
- [ ] `FFI.ForeignPtr.Nat8.allocate` (foreign FFI_ForeignPtr_Nat8_allocate)
- [ ] `FFI.ForeignPtr.Nat16.allocate` (foreign FFI_ForeignPtr_Nat16_allocate)
- [ ] `FFI.ForeignPtr.Nat32.allocate` (foreign FFI_ForeignPtr_Nat32_allocate)
- [ ] `FFI.ForeignPtr.Nat.allocate` (foreign FFI_ForeignPtr_Nat_allocate)
- [ ] `FFI.ForeignPtr.Float32.allocate` (foreign FFI_ForeignPtr_Float32_allocate)
- [ ] `FFI.ForeignPtr.Float.allocate` (foreign FFI_ForeignPtr_Float_allocate)
- [ ] `FFI.ForeignPtr.Ptr.allocate` (no runtime implementation found)
