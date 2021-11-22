
# Squash merges

`squash src dest` merges can be used to merge from `src` to `dest`, discarding the history of `src`. It's useful when the source namespace history is irrelevant or has a bunch of churn you wish to discard. Often when merging small pull requests, you'll use a squash merge.

Let's look at some examples. We'll start with a namespace with just the builtins. Let's take a look at the hash of this namespace:

```ucm
.> history builtin

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #pmfe8arl5l
  
    + Adds / updates:
    
      Any Any.Any Any.unsafeExtract Boolean Boolean.not bug
      Bytes Bytes.++ Bytes.at Bytes.decodeNat16be
      Bytes.decodeNat16le Bytes.decodeNat32be
      Bytes.decodeNat32le Bytes.decodeNat64be
      Bytes.decodeNat64le Bytes.drop Bytes.empty
      Bytes.encodeNat16be Bytes.encodeNat16le
      Bytes.encodeNat32be Bytes.encodeNat32le
      Bytes.encodeNat64be Bytes.encodeNat64le Bytes.flatten
      Bytes.fromBase16 Bytes.fromBase32 Bytes.fromBase64
      Bytes.fromBase64UrlUnpadded Bytes.fromList
      Bytes.gzip.compress Bytes.gzip.decompress Bytes.size
      Bytes.take Bytes.toBase16 Bytes.toBase32 Bytes.toBase64
      Bytes.toBase64UrlUnpadded Bytes.toList Bytes.zlib.compress
      Bytes.zlib.decompress Char Char.fromNat Char.toNat
      Char.toText Code Code.cache_ Code.dependencies
      Code.deserialize Code.display Code.isMissing Code.lookup
      Code.serialize Code.validate crypto.hash
      crypto.HashAlgorithm crypto.HashAlgorithm.Blake2b_256
      crypto.HashAlgorithm.Blake2b_512
      crypto.HashAlgorithm.Blake2s_256
      crypto.HashAlgorithm.Sha2_256
      crypto.HashAlgorithm.Sha2_512
      crypto.HashAlgorithm.Sha3_256
      crypto.HashAlgorithm.Sha3_512 crypto.hashBytes crypto.hmac
      crypto.hmacBytes Debug.watch Doc Doc.Blob Doc.Evaluate
      Doc.Join Doc.Link Doc.Signature Doc.Source Either
      Either.Left Either.Right Exception Exception.raise Float
      Float.* Float.+ Float.- Float./ Float.abs Float.acos
      Float.acosh Float.asin Float.asinh Float.atan Float.atan2
      Float.atanh Float.ceiling Float.cos Float.cosh Float.eq
      Float.exp Float.floor Float.fromRepresentation
      Float.fromText Float.gt Float.gteq Float.log Float.logBase
      Float.lt Float.lteq Float.max Float.min Float.pow
      Float.round Float.sin Float.sinh Float.sqrt Float.tan
      Float.tanh Float.toRepresentation Float.toText
      Float.truncate Int Int.* Int.+ Int.- Int./ Int.and
      Int.complement Int.eq Int.fromRepresentation Int.fromText
      Int.gt Int.gteq Int.increment Int.isEven Int.isOdd
      Int.leadingZeros Int.lt Int.lteq Int.mod Int.negate Int.or
      Int.popCount Int.pow Int.shiftLeft Int.shiftRight
      Int.signum Int.toFloat Int.toRepresentation Int.toText
      Int.trailingZeros Int.truncate0 Int.xor io2.BufferMode
      io2.BufferMode.BlockBuffering io2.BufferMode.LineBuffering
      io2.BufferMode.NoBuffering
      io2.BufferMode.SizedBlockBuffering io2.Failure
      io2.Failure.Failure io2.FileMode io2.FileMode.Append
      io2.FileMode.Read io2.FileMode.ReadWrite
      io2.FileMode.Write io2.Handle io2.IO
      io2.IO.clientSocket.impl io2.IO.closeFile.impl
      io2.IO.closeSocket.impl io2.IO.createDirectory.impl
      io2.IO.createTempDirectory.impl io2.IO.delay.impl
      io2.IO.directoryContents.impl io2.IO.fileExists.impl
      io2.IO.forkComp io2.IO.getArgs.impl
      io2.IO.getBuffering.impl io2.IO.getBytes.impl
      io2.IO.getCurrentDirectory.impl io2.IO.getEnv.impl
      io2.IO.getFileSize.impl io2.IO.getFileTimestamp.impl
      io2.IO.getLine.impl io2.IO.getTempDirectory.impl
      io2.IO.handlePosition.impl io2.IO.isDirectory.impl
      io2.IO.isFileEOF.impl io2.IO.isFileOpen.impl
      io2.IO.isSeekable.impl io2.IO.kill.impl io2.IO.listen.impl
      io2.IO.openFile.impl io2.IO.putBytes.impl io2.IO.ref
      io2.IO.removeDirectory.impl io2.IO.removeFile.impl
      io2.IO.renameDirectory.impl io2.IO.renameFile.impl
      io2.IO.seekHandle.impl io2.IO.serverSocket.impl
      io2.IO.setBuffering.impl io2.IO.setCurrentDirectory.impl
      io2.IO.socketAccept.impl io2.IO.socketPort.impl
      io2.IO.socketReceive.impl io2.IO.socketSend.impl
      io2.IO.stdHandle io2.IO.systemTime.impl
      io2.IO.systemTimeMicroseconds io2.IOError
      io2.IOError.AlreadyExists io2.IOError.EOF
      io2.IOError.IllegalOperation io2.IOError.NoSuchThing
      io2.IOError.PermissionDenied io2.IOError.ResourceBusy
      io2.IOError.ResourceExhausted io2.IOError.UserError
      io2.IOFailure io2.MVar io2.MVar.isEmpty io2.MVar.new
      io2.MVar.newEmpty io2.MVar.put.impl io2.MVar.read.impl
      io2.MVar.swap.impl io2.MVar.take.impl io2.MVar.tryPut.impl
      io2.MVar.tryRead.impl io2.MVar.tryTake io2.SeekMode
      io2.SeekMode.AbsoluteSeek io2.SeekMode.RelativeSeek
      io2.SeekMode.SeekFromEnd io2.Socket io2.StdHandle
      io2.StdHandle.StdErr io2.StdHandle.StdIn
      io2.StdHandle.StdOut io2.STM io2.STM.atomically
      io2.STM.retry io2.ThreadId io2.Tls io2.Tls.Cipher
      io2.Tls.ClientConfig io2.Tls.ClientConfig.certificates.set
      io2.TLS.ClientConfig.ciphers.set
      io2.Tls.ClientConfig.default
      io2.Tls.ClientConfig.versions.set io2.Tls.decodeCert.impl
      io2.Tls.decodePrivateKey io2.Tls.encodeCert
      io2.Tls.encodePrivateKey io2.Tls.handshake.impl
      io2.Tls.newClient.impl io2.Tls.newServer.impl
      io2.Tls.PrivateKey io2.Tls.receive.impl io2.Tls.send.impl
      io2.Tls.ServerConfig io2.Tls.ServerConfig.certificates.set
      io2.Tls.ServerConfig.ciphers.set
      io2.Tls.ServerConfig.default
      io2.Tls.ServerConfig.versions.set io2.Tls.SignedCert
      io2.Tls.terminate.impl io2.Tls.Version io2.TlsFailure
      io2.TVar io2.TVar.new io2.TVar.newIO io2.TVar.read
      io2.TVar.readIO io2.TVar.swap io2.TVar.write IsPropagated
      IsPropagated.IsPropagated IsTest IsTest.IsTest Link
      Link.Term.toText List List.++ List.+: List.:+ List.at
      List.cons List.drop List.empty List.size List.snoc
      List.take metadata.isPropagated metadata.isTest Nat Nat.*
      Nat.+ Nat./ Nat.and Nat.complement Nat.drop Nat.eq
      Nat.fromText Nat.gt Nat.gteq Nat.increment Nat.isEven
      Nat.isOdd Nat.leadingZeros Nat.lt Nat.lteq Nat.mod Nat.or
      Nat.popCount Nat.pow Nat.shiftLeft Nat.shiftRight Nat.sub
      Nat.toFloat Nat.toInt Nat.toText Nat.trailingZeros Nat.xor
      Optional Optional.None Optional.Some Ref Ref.read
      Ref.write Request Scope Scope.ref Scope.run SeqView
      SeqView.VElem SeqView.VEmpty Test.Result Test.Result.Fail
      Test.Result.Ok Text Text.!= Text.++ Text.drop Text.empty
      Text.eq Text.fromCharList Text.fromUtf8.impl Text.gt
      Text.gteq Text.lt Text.lteq Text.repeat Text.size
      Text.take Text.toCharList Text.toUtf8 Text.uncons
      Text.unsnoc todo Tuple Tuple.Cons Unit Unit.Unit
      Universal.< Universal.<= Universal.== Universal.>
      Universal.>= Universal.compare unsafe.coerceAbilities
      Value Value.dependencies Value.deserialize Value.load
      Value.serialize Value.value Link.Term##Link.Term
      Link.Term#quh#0 Link.Type##Link.Type Link.Type#quh#1
  
  □ #sjg2v58vn2 (start of history)

.> fork builtin builtin2

  Done.

```
(We make a copy of `builtin` for use later in this transcript.)

Now suppose we `fork` a copy of builtin, then rename `Nat.+` to `frobnicate`, then rename it back. Notice this produces multiple entries in the history:

```ucm
.> fork builtin mybuiltin

  Done.

.mybuiltin> rename.term Nat.+ Nat.frobnicate

  Done.

.mybuiltin> rename.term Nat.frobnicate Nat.+

  Done.

.mybuiltin> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #tkogb4t61n
  
    > Moves:
    
      Original name  New name
      Nat.frobnicate Nat.+
  
  ⊙ #ns4n9uvs99
  
    > Moves:
    
      Original name New name
      Nat.+         Nat.frobnicate
  
  ⊙ #pmfe8arl5l
  
    + Adds / updates:
    
      Any Any.Any Any.unsafeExtract Boolean Boolean.not bug
      Bytes Bytes.++ Bytes.at Bytes.decodeNat16be
      Bytes.decodeNat16le Bytes.decodeNat32be
      Bytes.decodeNat32le Bytes.decodeNat64be
      Bytes.decodeNat64le Bytes.drop Bytes.empty
      Bytes.encodeNat16be Bytes.encodeNat16le
      Bytes.encodeNat32be Bytes.encodeNat32le
      Bytes.encodeNat64be Bytes.encodeNat64le Bytes.flatten
      Bytes.fromBase16 Bytes.fromBase32 Bytes.fromBase64
      Bytes.fromBase64UrlUnpadded Bytes.fromList
      Bytes.gzip.compress Bytes.gzip.decompress Bytes.size
      Bytes.take Bytes.toBase16 Bytes.toBase32 Bytes.toBase64
      Bytes.toBase64UrlUnpadded Bytes.toList Bytes.zlib.compress
      Bytes.zlib.decompress Char Char.fromNat Char.toNat
      Char.toText Code Code.cache_ Code.dependencies
      Code.deserialize Code.display Code.isMissing Code.lookup
      Code.serialize Code.validate crypto.hash
      crypto.HashAlgorithm crypto.HashAlgorithm.Blake2b_256
      crypto.HashAlgorithm.Blake2b_512
      crypto.HashAlgorithm.Blake2s_256
      crypto.HashAlgorithm.Sha2_256
      crypto.HashAlgorithm.Sha2_512
      crypto.HashAlgorithm.Sha3_256
      crypto.HashAlgorithm.Sha3_512 crypto.hashBytes crypto.hmac
      crypto.hmacBytes Debug.watch Doc Doc.Blob Doc.Evaluate
      Doc.Join Doc.Link Doc.Signature Doc.Source Either
      Either.Left Either.Right Exception Exception.raise Float
      Float.* Float.+ Float.- Float./ Float.abs Float.acos
      Float.acosh Float.asin Float.asinh Float.atan Float.atan2
      Float.atanh Float.ceiling Float.cos Float.cosh Float.eq
      Float.exp Float.floor Float.fromRepresentation
      Float.fromText Float.gt Float.gteq Float.log Float.logBase
      Float.lt Float.lteq Float.max Float.min Float.pow
      Float.round Float.sin Float.sinh Float.sqrt Float.tan
      Float.tanh Float.toRepresentation Float.toText
      Float.truncate Int Int.* Int.+ Int.- Int./ Int.and
      Int.complement Int.eq Int.fromRepresentation Int.fromText
      Int.gt Int.gteq Int.increment Int.isEven Int.isOdd
      Int.leadingZeros Int.lt Int.lteq Int.mod Int.negate Int.or
      Int.popCount Int.pow Int.shiftLeft Int.shiftRight
      Int.signum Int.toFloat Int.toRepresentation Int.toText
      Int.trailingZeros Int.truncate0 Int.xor io2.BufferMode
      io2.BufferMode.BlockBuffering io2.BufferMode.LineBuffering
      io2.BufferMode.NoBuffering
      io2.BufferMode.SizedBlockBuffering io2.Failure
      io2.Failure.Failure io2.FileMode io2.FileMode.Append
      io2.FileMode.Read io2.FileMode.ReadWrite
      io2.FileMode.Write io2.Handle io2.IO
      io2.IO.clientSocket.impl io2.IO.closeFile.impl
      io2.IO.closeSocket.impl io2.IO.createDirectory.impl
      io2.IO.createTempDirectory.impl io2.IO.delay.impl
      io2.IO.directoryContents.impl io2.IO.fileExists.impl
      io2.IO.forkComp io2.IO.getArgs.impl
      io2.IO.getBuffering.impl io2.IO.getBytes.impl
      io2.IO.getCurrentDirectory.impl io2.IO.getEnv.impl
      io2.IO.getFileSize.impl io2.IO.getFileTimestamp.impl
      io2.IO.getLine.impl io2.IO.getTempDirectory.impl
      io2.IO.handlePosition.impl io2.IO.isDirectory.impl
      io2.IO.isFileEOF.impl io2.IO.isFileOpen.impl
      io2.IO.isSeekable.impl io2.IO.kill.impl io2.IO.listen.impl
      io2.IO.openFile.impl io2.IO.putBytes.impl io2.IO.ref
      io2.IO.removeDirectory.impl io2.IO.removeFile.impl
      io2.IO.renameDirectory.impl io2.IO.renameFile.impl
      io2.IO.seekHandle.impl io2.IO.serverSocket.impl
      io2.IO.setBuffering.impl io2.IO.setCurrentDirectory.impl
      io2.IO.socketAccept.impl io2.IO.socketPort.impl
      io2.IO.socketReceive.impl io2.IO.socketSend.impl
      io2.IO.stdHandle io2.IO.systemTime.impl
      io2.IO.systemTimeMicroseconds io2.IOError
      io2.IOError.AlreadyExists io2.IOError.EOF
      io2.IOError.IllegalOperation io2.IOError.NoSuchThing
      io2.IOError.PermissionDenied io2.IOError.ResourceBusy
      io2.IOError.ResourceExhausted io2.IOError.UserError
      io2.IOFailure io2.MVar io2.MVar.isEmpty io2.MVar.new
      io2.MVar.newEmpty io2.MVar.put.impl io2.MVar.read.impl
      io2.MVar.swap.impl io2.MVar.take.impl io2.MVar.tryPut.impl
      io2.MVar.tryRead.impl io2.MVar.tryTake io2.SeekMode
      io2.SeekMode.AbsoluteSeek io2.SeekMode.RelativeSeek
      io2.SeekMode.SeekFromEnd io2.Socket io2.StdHandle
      io2.StdHandle.StdErr io2.StdHandle.StdIn
      io2.StdHandle.StdOut io2.STM io2.STM.atomically
      io2.STM.retry io2.ThreadId io2.Tls io2.Tls.Cipher
      io2.Tls.ClientConfig io2.Tls.ClientConfig.certificates.set
      io2.TLS.ClientConfig.ciphers.set
      io2.Tls.ClientConfig.default
      io2.Tls.ClientConfig.versions.set io2.Tls.decodeCert.impl
      io2.Tls.decodePrivateKey io2.Tls.encodeCert
      io2.Tls.encodePrivateKey io2.Tls.handshake.impl
      io2.Tls.newClient.impl io2.Tls.newServer.impl
      io2.Tls.PrivateKey io2.Tls.receive.impl io2.Tls.send.impl
      io2.Tls.ServerConfig io2.Tls.ServerConfig.certificates.set
      io2.Tls.ServerConfig.ciphers.set
      io2.Tls.ServerConfig.default
      io2.Tls.ServerConfig.versions.set io2.Tls.SignedCert
      io2.Tls.terminate.impl io2.Tls.Version io2.TlsFailure
      io2.TVar io2.TVar.new io2.TVar.newIO io2.TVar.read
      io2.TVar.readIO io2.TVar.swap io2.TVar.write IsPropagated
      IsPropagated.IsPropagated IsTest IsTest.IsTest Link
      Link.Term.toText List List.++ List.+: List.:+ List.at
      List.cons List.drop List.empty List.size List.snoc
      List.take metadata.isPropagated metadata.isTest Nat Nat.*
      Nat.+ Nat./ Nat.and Nat.complement Nat.drop Nat.eq
      Nat.fromText Nat.gt Nat.gteq Nat.increment Nat.isEven
      Nat.isOdd Nat.leadingZeros Nat.lt Nat.lteq Nat.mod Nat.or
      Nat.popCount Nat.pow Nat.shiftLeft Nat.shiftRight Nat.sub
      Nat.toFloat Nat.toInt Nat.toText Nat.trailingZeros Nat.xor
      Optional Optional.None Optional.Some Ref Ref.read
      Ref.write Request Scope Scope.ref Scope.run SeqView
      SeqView.VElem SeqView.VEmpty Test.Result Test.Result.Fail
      Test.Result.Ok Text Text.!= Text.++ Text.drop Text.empty
      Text.eq Text.fromCharList Text.fromUtf8.impl Text.gt
      Text.gteq Text.lt Text.lteq Text.repeat Text.size
      Text.take Text.toCharList Text.toUtf8 Text.uncons
      Text.unsnoc todo Tuple Tuple.Cons Unit Unit.Unit
      Universal.< Universal.<= Universal.== Universal.>
      Universal.>= Universal.compare unsafe.coerceAbilities
      Value Value.dependencies Value.deserialize Value.load
      Value.serialize Value.value Link.Term##Link.Term
      Link.Term#quh#0 Link.Type##Link.Type Link.Type#quh#1
  
  □ #sjg2v58vn2 (start of history)

```
If we merge that back into `builtin`, we get that same chain of history:

```ucm
.> merge mybuiltin builtin

  Nothing changed as a result of the merge.

.> history builtin

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #tkogb4t61n
  
    > Moves:
    
      Original name  New name
      Nat.frobnicate Nat.+
  
  ⊙ #ns4n9uvs99
  
    > Moves:
    
      Original name New name
      Nat.+         Nat.frobnicate
  
  ⊙ #pmfe8arl5l
  
    + Adds / updates:
    
      Any Any.Any Any.unsafeExtract Boolean Boolean.not bug
      Bytes Bytes.++ Bytes.at Bytes.decodeNat16be
      Bytes.decodeNat16le Bytes.decodeNat32be
      Bytes.decodeNat32le Bytes.decodeNat64be
      Bytes.decodeNat64le Bytes.drop Bytes.empty
      Bytes.encodeNat16be Bytes.encodeNat16le
      Bytes.encodeNat32be Bytes.encodeNat32le
      Bytes.encodeNat64be Bytes.encodeNat64le Bytes.flatten
      Bytes.fromBase16 Bytes.fromBase32 Bytes.fromBase64
      Bytes.fromBase64UrlUnpadded Bytes.fromList
      Bytes.gzip.compress Bytes.gzip.decompress Bytes.size
      Bytes.take Bytes.toBase16 Bytes.toBase32 Bytes.toBase64
      Bytes.toBase64UrlUnpadded Bytes.toList Bytes.zlib.compress
      Bytes.zlib.decompress Char Char.fromNat Char.toNat
      Char.toText Code Code.cache_ Code.dependencies
      Code.deserialize Code.display Code.isMissing Code.lookup
      Code.serialize Code.validate crypto.hash
      crypto.HashAlgorithm crypto.HashAlgorithm.Blake2b_256
      crypto.HashAlgorithm.Blake2b_512
      crypto.HashAlgorithm.Blake2s_256
      crypto.HashAlgorithm.Sha2_256
      crypto.HashAlgorithm.Sha2_512
      crypto.HashAlgorithm.Sha3_256
      crypto.HashAlgorithm.Sha3_512 crypto.hashBytes crypto.hmac
      crypto.hmacBytes Debug.watch Doc Doc.Blob Doc.Evaluate
      Doc.Join Doc.Link Doc.Signature Doc.Source Either
      Either.Left Either.Right Exception Exception.raise Float
      Float.* Float.+ Float.- Float./ Float.abs Float.acos
      Float.acosh Float.asin Float.asinh Float.atan Float.atan2
      Float.atanh Float.ceiling Float.cos Float.cosh Float.eq
      Float.exp Float.floor Float.fromRepresentation
      Float.fromText Float.gt Float.gteq Float.log Float.logBase
      Float.lt Float.lteq Float.max Float.min Float.pow
      Float.round Float.sin Float.sinh Float.sqrt Float.tan
      Float.tanh Float.toRepresentation Float.toText
      Float.truncate Int Int.* Int.+ Int.- Int./ Int.and
      Int.complement Int.eq Int.fromRepresentation Int.fromText
      Int.gt Int.gteq Int.increment Int.isEven Int.isOdd
      Int.leadingZeros Int.lt Int.lteq Int.mod Int.negate Int.or
      Int.popCount Int.pow Int.shiftLeft Int.shiftRight
      Int.signum Int.toFloat Int.toRepresentation Int.toText
      Int.trailingZeros Int.truncate0 Int.xor io2.BufferMode
      io2.BufferMode.BlockBuffering io2.BufferMode.LineBuffering
      io2.BufferMode.NoBuffering
      io2.BufferMode.SizedBlockBuffering io2.Failure
      io2.Failure.Failure io2.FileMode io2.FileMode.Append
      io2.FileMode.Read io2.FileMode.ReadWrite
      io2.FileMode.Write io2.Handle io2.IO
      io2.IO.clientSocket.impl io2.IO.closeFile.impl
      io2.IO.closeSocket.impl io2.IO.createDirectory.impl
      io2.IO.createTempDirectory.impl io2.IO.delay.impl
      io2.IO.directoryContents.impl io2.IO.fileExists.impl
      io2.IO.forkComp io2.IO.getArgs.impl
      io2.IO.getBuffering.impl io2.IO.getBytes.impl
      io2.IO.getCurrentDirectory.impl io2.IO.getEnv.impl
      io2.IO.getFileSize.impl io2.IO.getFileTimestamp.impl
      io2.IO.getLine.impl io2.IO.getTempDirectory.impl
      io2.IO.handlePosition.impl io2.IO.isDirectory.impl
      io2.IO.isFileEOF.impl io2.IO.isFileOpen.impl
      io2.IO.isSeekable.impl io2.IO.kill.impl io2.IO.listen.impl
      io2.IO.openFile.impl io2.IO.putBytes.impl io2.IO.ref
      io2.IO.removeDirectory.impl io2.IO.removeFile.impl
      io2.IO.renameDirectory.impl io2.IO.renameFile.impl
      io2.IO.seekHandle.impl io2.IO.serverSocket.impl
      io2.IO.setBuffering.impl io2.IO.setCurrentDirectory.impl
      io2.IO.socketAccept.impl io2.IO.socketPort.impl
      io2.IO.socketReceive.impl io2.IO.socketSend.impl
      io2.IO.stdHandle io2.IO.systemTime.impl
      io2.IO.systemTimeMicroseconds io2.IOError
      io2.IOError.AlreadyExists io2.IOError.EOF
      io2.IOError.IllegalOperation io2.IOError.NoSuchThing
      io2.IOError.PermissionDenied io2.IOError.ResourceBusy
      io2.IOError.ResourceExhausted io2.IOError.UserError
      io2.IOFailure io2.MVar io2.MVar.isEmpty io2.MVar.new
      io2.MVar.newEmpty io2.MVar.put.impl io2.MVar.read.impl
      io2.MVar.swap.impl io2.MVar.take.impl io2.MVar.tryPut.impl
      io2.MVar.tryRead.impl io2.MVar.tryTake io2.SeekMode
      io2.SeekMode.AbsoluteSeek io2.SeekMode.RelativeSeek
      io2.SeekMode.SeekFromEnd io2.Socket io2.StdHandle
      io2.StdHandle.StdErr io2.StdHandle.StdIn
      io2.StdHandle.StdOut io2.STM io2.STM.atomically
      io2.STM.retry io2.ThreadId io2.Tls io2.Tls.Cipher
      io2.Tls.ClientConfig io2.Tls.ClientConfig.certificates.set
      io2.TLS.ClientConfig.ciphers.set
      io2.Tls.ClientConfig.default
      io2.Tls.ClientConfig.versions.set io2.Tls.decodeCert.impl
      io2.Tls.decodePrivateKey io2.Tls.encodeCert
      io2.Tls.encodePrivateKey io2.Tls.handshake.impl
      io2.Tls.newClient.impl io2.Tls.newServer.impl
      io2.Tls.PrivateKey io2.Tls.receive.impl io2.Tls.send.impl
      io2.Tls.ServerConfig io2.Tls.ServerConfig.certificates.set
      io2.Tls.ServerConfig.ciphers.set
      io2.Tls.ServerConfig.default
      io2.Tls.ServerConfig.versions.set io2.Tls.SignedCert
      io2.Tls.terminate.impl io2.Tls.Version io2.TlsFailure
      io2.TVar io2.TVar.new io2.TVar.newIO io2.TVar.read
      io2.TVar.readIO io2.TVar.swap io2.TVar.write IsPropagated
      IsPropagated.IsPropagated IsTest IsTest.IsTest Link
      Link.Term.toText List List.++ List.+: List.:+ List.at
      List.cons List.drop List.empty List.size List.snoc
      List.take metadata.isPropagated metadata.isTest Nat Nat.*
      Nat.+ Nat./ Nat.and Nat.complement Nat.drop Nat.eq
      Nat.fromText Nat.gt Nat.gteq Nat.increment Nat.isEven
      Nat.isOdd Nat.leadingZeros Nat.lt Nat.lteq Nat.mod Nat.or
      Nat.popCount Nat.pow Nat.shiftLeft Nat.shiftRight Nat.sub
      Nat.toFloat Nat.toInt Nat.toText Nat.trailingZeros Nat.xor
      Optional Optional.None Optional.Some Ref Ref.read
      Ref.write Request Scope Scope.ref Scope.run SeqView
      SeqView.VElem SeqView.VEmpty Test.Result Test.Result.Fail
      Test.Result.Ok Text Text.!= Text.++ Text.drop Text.empty
      Text.eq Text.fromCharList Text.fromUtf8.impl Text.gt
      Text.gteq Text.lt Text.lteq Text.repeat Text.size
      Text.take Text.toCharList Text.toUtf8 Text.uncons
      Text.unsnoc todo Tuple Tuple.Cons Unit Unit.Unit
      Universal.< Universal.<= Universal.== Universal.>
      Universal.>= Universal.compare unsafe.coerceAbilities
      Value Value.dependencies Value.deserialize Value.load
      Value.serialize Value.value Link.Term##Link.Term
      Link.Term#quh#0 Link.Type##Link.Type Link.Type#quh#1
  
  □ #sjg2v58vn2 (start of history)

```
Let's try again, but using a `merge.squash` (or just `squash`) instead. The history will be unchanged:

```ucm
.> merge.squash mybuiltin builtin2

  Nothing changed as a result of the merge.

  😶
  
  builtin2 was already up-to-date with mybuiltin.

.> history builtin2

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #pmfe8arl5l
  
    + Adds / updates:
    
      Any Any.Any Any.unsafeExtract Boolean Boolean.not bug
      Bytes Bytes.++ Bytes.at Bytes.decodeNat16be
      Bytes.decodeNat16le Bytes.decodeNat32be
      Bytes.decodeNat32le Bytes.decodeNat64be
      Bytes.decodeNat64le Bytes.drop Bytes.empty
      Bytes.encodeNat16be Bytes.encodeNat16le
      Bytes.encodeNat32be Bytes.encodeNat32le
      Bytes.encodeNat64be Bytes.encodeNat64le Bytes.flatten
      Bytes.fromBase16 Bytes.fromBase32 Bytes.fromBase64
      Bytes.fromBase64UrlUnpadded Bytes.fromList
      Bytes.gzip.compress Bytes.gzip.decompress Bytes.size
      Bytes.take Bytes.toBase16 Bytes.toBase32 Bytes.toBase64
      Bytes.toBase64UrlUnpadded Bytes.toList Bytes.zlib.compress
      Bytes.zlib.decompress Char Char.fromNat Char.toNat
      Char.toText Code Code.cache_ Code.dependencies
      Code.deserialize Code.display Code.isMissing Code.lookup
      Code.serialize Code.validate crypto.hash
      crypto.HashAlgorithm crypto.HashAlgorithm.Blake2b_256
      crypto.HashAlgorithm.Blake2b_512
      crypto.HashAlgorithm.Blake2s_256
      crypto.HashAlgorithm.Sha2_256
      crypto.HashAlgorithm.Sha2_512
      crypto.HashAlgorithm.Sha3_256
      crypto.HashAlgorithm.Sha3_512 crypto.hashBytes crypto.hmac
      crypto.hmacBytes Debug.watch Doc Doc.Blob Doc.Evaluate
      Doc.Join Doc.Link Doc.Signature Doc.Source Either
      Either.Left Either.Right Exception Exception.raise Float
      Float.* Float.+ Float.- Float./ Float.abs Float.acos
      Float.acosh Float.asin Float.asinh Float.atan Float.atan2
      Float.atanh Float.ceiling Float.cos Float.cosh Float.eq
      Float.exp Float.floor Float.fromRepresentation
      Float.fromText Float.gt Float.gteq Float.log Float.logBase
      Float.lt Float.lteq Float.max Float.min Float.pow
      Float.round Float.sin Float.sinh Float.sqrt Float.tan
      Float.tanh Float.toRepresentation Float.toText
      Float.truncate Int Int.* Int.+ Int.- Int./ Int.and
      Int.complement Int.eq Int.fromRepresentation Int.fromText
      Int.gt Int.gteq Int.increment Int.isEven Int.isOdd
      Int.leadingZeros Int.lt Int.lteq Int.mod Int.negate Int.or
      Int.popCount Int.pow Int.shiftLeft Int.shiftRight
      Int.signum Int.toFloat Int.toRepresentation Int.toText
      Int.trailingZeros Int.truncate0 Int.xor io2.BufferMode
      io2.BufferMode.BlockBuffering io2.BufferMode.LineBuffering
      io2.BufferMode.NoBuffering
      io2.BufferMode.SizedBlockBuffering io2.Failure
      io2.Failure.Failure io2.FileMode io2.FileMode.Append
      io2.FileMode.Read io2.FileMode.ReadWrite
      io2.FileMode.Write io2.Handle io2.IO
      io2.IO.clientSocket.impl io2.IO.closeFile.impl
      io2.IO.closeSocket.impl io2.IO.createDirectory.impl
      io2.IO.createTempDirectory.impl io2.IO.delay.impl
      io2.IO.directoryContents.impl io2.IO.fileExists.impl
      io2.IO.forkComp io2.IO.getArgs.impl
      io2.IO.getBuffering.impl io2.IO.getBytes.impl
      io2.IO.getCurrentDirectory.impl io2.IO.getEnv.impl
      io2.IO.getFileSize.impl io2.IO.getFileTimestamp.impl
      io2.IO.getLine.impl io2.IO.getTempDirectory.impl
      io2.IO.handlePosition.impl io2.IO.isDirectory.impl
      io2.IO.isFileEOF.impl io2.IO.isFileOpen.impl
      io2.IO.isSeekable.impl io2.IO.kill.impl io2.IO.listen.impl
      io2.IO.openFile.impl io2.IO.putBytes.impl io2.IO.ref
      io2.IO.removeDirectory.impl io2.IO.removeFile.impl
      io2.IO.renameDirectory.impl io2.IO.renameFile.impl
      io2.IO.seekHandle.impl io2.IO.serverSocket.impl
      io2.IO.setBuffering.impl io2.IO.setCurrentDirectory.impl
      io2.IO.socketAccept.impl io2.IO.socketPort.impl
      io2.IO.socketReceive.impl io2.IO.socketSend.impl
      io2.IO.stdHandle io2.IO.systemTime.impl
      io2.IO.systemTimeMicroseconds io2.IOError
      io2.IOError.AlreadyExists io2.IOError.EOF
      io2.IOError.IllegalOperation io2.IOError.NoSuchThing
      io2.IOError.PermissionDenied io2.IOError.ResourceBusy
      io2.IOError.ResourceExhausted io2.IOError.UserError
      io2.IOFailure io2.MVar io2.MVar.isEmpty io2.MVar.new
      io2.MVar.newEmpty io2.MVar.put.impl io2.MVar.read.impl
      io2.MVar.swap.impl io2.MVar.take.impl io2.MVar.tryPut.impl
      io2.MVar.tryRead.impl io2.MVar.tryTake io2.SeekMode
      io2.SeekMode.AbsoluteSeek io2.SeekMode.RelativeSeek
      io2.SeekMode.SeekFromEnd io2.Socket io2.StdHandle
      io2.StdHandle.StdErr io2.StdHandle.StdIn
      io2.StdHandle.StdOut io2.STM io2.STM.atomically
      io2.STM.retry io2.ThreadId io2.Tls io2.Tls.Cipher
      io2.Tls.ClientConfig io2.Tls.ClientConfig.certificates.set
      io2.TLS.ClientConfig.ciphers.set
      io2.Tls.ClientConfig.default
      io2.Tls.ClientConfig.versions.set io2.Tls.decodeCert.impl
      io2.Tls.decodePrivateKey io2.Tls.encodeCert
      io2.Tls.encodePrivateKey io2.Tls.handshake.impl
      io2.Tls.newClient.impl io2.Tls.newServer.impl
      io2.Tls.PrivateKey io2.Tls.receive.impl io2.Tls.send.impl
      io2.Tls.ServerConfig io2.Tls.ServerConfig.certificates.set
      io2.Tls.ServerConfig.ciphers.set
      io2.Tls.ServerConfig.default
      io2.Tls.ServerConfig.versions.set io2.Tls.SignedCert
      io2.Tls.terminate.impl io2.Tls.Version io2.TlsFailure
      io2.TVar io2.TVar.new io2.TVar.newIO io2.TVar.read
      io2.TVar.readIO io2.TVar.swap io2.TVar.write IsPropagated
      IsPropagated.IsPropagated IsTest IsTest.IsTest Link
      Link.Term.toText List List.++ List.+: List.:+ List.at
      List.cons List.drop List.empty List.size List.snoc
      List.take metadata.isPropagated metadata.isTest Nat Nat.*
      Nat.+ Nat./ Nat.and Nat.complement Nat.drop Nat.eq
      Nat.fromText Nat.gt Nat.gteq Nat.increment Nat.isEven
      Nat.isOdd Nat.leadingZeros Nat.lt Nat.lteq Nat.mod Nat.or
      Nat.popCount Nat.pow Nat.shiftLeft Nat.shiftRight Nat.sub
      Nat.toFloat Nat.toInt Nat.toText Nat.trailingZeros Nat.xor
      Optional Optional.None Optional.Some Ref Ref.read
      Ref.write Request Scope Scope.ref Scope.run SeqView
      SeqView.VElem SeqView.VEmpty Test.Result Test.Result.Fail
      Test.Result.Ok Text Text.!= Text.++ Text.drop Text.empty
      Text.eq Text.fromCharList Text.fromUtf8.impl Text.gt
      Text.gteq Text.lt Text.lteq Text.repeat Text.size
      Text.take Text.toCharList Text.toUtf8 Text.uncons
      Text.unsnoc todo Tuple Tuple.Cons Unit Unit.Unit
      Universal.< Universal.<= Universal.== Universal.>
      Universal.>= Universal.compare unsafe.coerceAbilities
      Value Value.dependencies Value.deserialize Value.load
      Value.serialize Value.value Link.Term##Link.Term
      Link.Term#quh#0 Link.Type##Link.Type Link.Type#quh#1
  
  □ #sjg2v58vn2 (start of history)

```
The churn that happened in `mybuiltin` namespace ended up back in the same spot, so the squash merge of that namespace with our original namespace had no effect.

## Another example

Let's look at a more interesting example, where the two namespaces have diverged a bit. Here's our starting namespace:

```unison
x = 1
```

```ucm
  ☝️  The namespace .trunk is empty.

.trunk> add

  ⍟ I've added these definitions:
  
    x : Nat

.> fork trunk alice

  Done.

.> fork trunk bob

  Done.

```
Alice now does some hacking:

```unison
radNumber = 348
bodaciousNumero = 2394
neatoFun x = x
```

```ucm
.alice> add

  ⍟ I've added these definitions:
  
    bodaciousNumero : Nat
    neatoFun        : x -> x
    radNumber       : Nat

.alice> rename.term radNumber superRadNumber

  Done.

.alice> rename.term neatoFun productionReadyId

  Done.

```
Meanwhile, Bob does his own hacking:

```unison
whatIsLove = "?"
babyDon'tHurtMe = ".. Don't hurt me..."
no more = no more
```

```ucm
.bob> add

  ⍟ I've added these definitions:
  
    babyDon'tHurtMe : Text
    no              : more -> r
    whatIsLove      : Text

```
At this point, Alice and Bob both have some history beyond what's in trunk:

```ucm
.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #hkrqt3tm05 (start of history)

.> history alice

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #uollchacf2
  
    > Moves:
    
      Original name New name
      neatoFun      productionReadyId
  
  ⊙ #7b6lii2lmc
  
    > Moves:
    
      Original name New name
      radNumber     superRadNumber
  
  ⊙ #1l7bsgu3om
  
    + Adds / updates:
    
      bodaciousNumero neatoFun radNumber
  
  □ #hkrqt3tm05 (start of history)

.> history bob

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #aicts31vr6
  
    + Adds / updates:
    
      babyDon'tHurtMe no whatIsLove
  
  □ #hkrqt3tm05 (start of history)

```
Alice then squash merges into `trunk`, as does Bob. It's as if Alice and Bob both made their changes in one single commit.

```ucm
.> merge.squash alice trunk

  Here's what's changed in trunk after the merge:
  
  Added definitions:
  
    1. bodaciousNumero   : Nat
    2. productionReadyId : x -> x
    3. superRadNumber    : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #gjfd096e1s
  
    + Adds / updates:
    
      bodaciousNumero productionReadyId superRadNumber
  
  □ #hkrqt3tm05 (start of history)

.> merge.squash bob trunk

  Here's what's changed in trunk after the merge:
  
  Added definitions:
  
    1. babyDon'tHurtMe : Text
    2. no              : more -> r
    3. whatIsLove      : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #k7bfk3l7uv
  
    + Adds / updates:
    
      babyDon'tHurtMe no whatIsLove
  
  ⊙ #gjfd096e1s
  
    + Adds / updates:
    
      bodaciousNumero productionReadyId superRadNumber
  
  □ #hkrqt3tm05 (start of history)

```
Since squash merges don't produce any merge nodes, we can `undo` a couple times to get back to our starting state:

```ucm
.> undo

  Here are the changes I undid
  
  Name changes:
  
    Original                  Changes
    1. bob.babyDon'tHurtMe    2. trunk.babyDon'tHurtMe (added)
    
    3. bob.no                 4. trunk.no (added)
    
    5. bob.whatIsLove         6. trunk.whatIsLove (added)

.> undo

  Here are the changes I undid
  
  Name changes:
  
    Original                      Changes
    1. alice.bodaciousNumero      2. trunk.bodaciousNumero (added)
    
    3. alice.productionReadyId    4. trunk.productionReadyId (added)
    
    5. alice.superRadNumber       6. trunk.superRadNumber (added)

.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #hkrqt3tm05 (start of history)

```
This time, we'll first squash Alice and Bob's changes together before squashing their combined changes into `trunk`. The resulting `trunk` will have just a single entry in it, combining both Alice and Bob's changes:

```ucm
.> squash alice bob

  Here's what's changed in bob after the merge:
  
  Added definitions:
  
    1. bodaciousNumero   : Nat
    2. productionReadyId : x -> x
    3. superRadNumber    : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> squash bob trunk

  Here's what's changed in trunk after the merge:
  
  Added definitions:
  
    1. babyDon'tHurtMe   : Text
    2. bodaciousNumero   : Nat
    3. no                : more -> r
    4. productionReadyId : x -> x
    5. superRadNumber    : Nat
    6. whatIsLove        : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #ka70nifphh
  
    + Adds / updates:
    
      babyDon'tHurtMe bodaciousNumero no productionReadyId
      superRadNumber whatIsLove
  
  □ #hkrqt3tm05 (start of history)

```
So, there you have it. With squashing, you can control the granularity of your history.

## Throwing out all history

Another thing we can do is `squash` into an empty namespace. This effectively makes a copy of the namespace, but without any of its history:

```ucm
.> squash alice nohistoryalice

  Here's what's changed in nohistoryalice after the merge:
  
  Added definitions:
  
    1. bodaciousNumero   : Nat
    2. productionReadyId : x -> x
    3. superRadNumber    : Nat
    4. x                 : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history nohistoryalice

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ #sui24env59 (start of history)

```
There's nothing really special here, `squash src dest` discards `src` history that comes after the LCA of `src` and `dest`, it's just that in the case of an empty namespace, that LCA is the beginning of time (the empty namespace), so all the history of `src` is discarded.

## Checking for handling of deletes

This checks to see that squashing correctly preserves deletions:

```ucm
  ☝️  The namespace .delete is empty.

.delete> builtins.merge

  Done.

.delete> fork builtin builtin2

  Done.

.delete> delete.term builtin2.Nat.+

  Name changes:
  
    Original                    Changes
    1. builtin.Nat.+         ┐  2. delete.builtin2.Nat.+ (removed)
    3. builtin2.Nat.+        │  
    4. delete.builtin.Nat.+  │  
    5. delete.builtin2.Nat.+ │  
    6. mybuiltin.Nat.+       ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

.delete> delete.term builtin2.Nat.*

  Name changes:
  
    Original                    Changes
    1. builtin.Nat.*         ┐  2. delete.builtin2.Nat.* (removed)
    3. builtin2.Nat.*        │  
    4. delete.builtin.Nat.*  │  
    5. delete.builtin2.Nat.* │  
    6. mybuiltin.Nat.*       ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

.delete> squash builtin2 builtin

  Here's what's changed in builtin after the merge:
  
  Removed definitions:
  
    1. Nat.* : Nat -> Nat -> Nat
    2. Nat.+ : Nat -> Nat -> Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.delete> history builtin

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #tdngh4la25
  
    - Deletes:
    
      Nat.* Nat.+
  
  ⊙ #pmfe8arl5l
  
    + Adds / updates:
    
      Any Any.Any Any.unsafeExtract Boolean Boolean.not bug
      Bytes Bytes.++ Bytes.at Bytes.decodeNat16be
      Bytes.decodeNat16le Bytes.decodeNat32be
      Bytes.decodeNat32le Bytes.decodeNat64be
      Bytes.decodeNat64le Bytes.drop Bytes.empty
      Bytes.encodeNat16be Bytes.encodeNat16le
      Bytes.encodeNat32be Bytes.encodeNat32le
      Bytes.encodeNat64be Bytes.encodeNat64le Bytes.flatten
      Bytes.fromBase16 Bytes.fromBase32 Bytes.fromBase64
      Bytes.fromBase64UrlUnpadded Bytes.fromList
      Bytes.gzip.compress Bytes.gzip.decompress Bytes.size
      Bytes.take Bytes.toBase16 Bytes.toBase32 Bytes.toBase64
      Bytes.toBase64UrlUnpadded Bytes.toList Bytes.zlib.compress
      Bytes.zlib.decompress Char Char.fromNat Char.toNat
      Char.toText Code Code.cache_ Code.dependencies
      Code.deserialize Code.display Code.isMissing Code.lookup
      Code.serialize Code.validate crypto.hash
      crypto.HashAlgorithm crypto.HashAlgorithm.Blake2b_256
      crypto.HashAlgorithm.Blake2b_512
      crypto.HashAlgorithm.Blake2s_256
      crypto.HashAlgorithm.Sha2_256
      crypto.HashAlgorithm.Sha2_512
      crypto.HashAlgorithm.Sha3_256
      crypto.HashAlgorithm.Sha3_512 crypto.hashBytes crypto.hmac
      crypto.hmacBytes Debug.watch Doc Doc.Blob Doc.Evaluate
      Doc.Join Doc.Link Doc.Signature Doc.Source Either
      Either.Left Either.Right Exception Exception.raise Float
      Float.* Float.+ Float.- Float./ Float.abs Float.acos
      Float.acosh Float.asin Float.asinh Float.atan Float.atan2
      Float.atanh Float.ceiling Float.cos Float.cosh Float.eq
      Float.exp Float.floor Float.fromRepresentation
      Float.fromText Float.gt Float.gteq Float.log Float.logBase
      Float.lt Float.lteq Float.max Float.min Float.pow
      Float.round Float.sin Float.sinh Float.sqrt Float.tan
      Float.tanh Float.toRepresentation Float.toText
      Float.truncate Int Int.* Int.+ Int.- Int./ Int.and
      Int.complement Int.eq Int.fromRepresentation Int.fromText
      Int.gt Int.gteq Int.increment Int.isEven Int.isOdd
      Int.leadingZeros Int.lt Int.lteq Int.mod Int.negate Int.or
      Int.popCount Int.pow Int.shiftLeft Int.shiftRight
      Int.signum Int.toFloat Int.toRepresentation Int.toText
      Int.trailingZeros Int.truncate0 Int.xor io2.BufferMode
      io2.BufferMode.BlockBuffering io2.BufferMode.LineBuffering
      io2.BufferMode.NoBuffering
      io2.BufferMode.SizedBlockBuffering io2.Failure
      io2.Failure.Failure io2.FileMode io2.FileMode.Append
      io2.FileMode.Read io2.FileMode.ReadWrite
      io2.FileMode.Write io2.Handle io2.IO
      io2.IO.clientSocket.impl io2.IO.closeFile.impl
      io2.IO.closeSocket.impl io2.IO.createDirectory.impl
      io2.IO.createTempDirectory.impl io2.IO.delay.impl
      io2.IO.directoryContents.impl io2.IO.fileExists.impl
      io2.IO.forkComp io2.IO.getArgs.impl
      io2.IO.getBuffering.impl io2.IO.getBytes.impl
      io2.IO.getCurrentDirectory.impl io2.IO.getEnv.impl
      io2.IO.getFileSize.impl io2.IO.getFileTimestamp.impl
      io2.IO.getLine.impl io2.IO.getTempDirectory.impl
      io2.IO.handlePosition.impl io2.IO.isDirectory.impl
      io2.IO.isFileEOF.impl io2.IO.isFileOpen.impl
      io2.IO.isSeekable.impl io2.IO.kill.impl io2.IO.listen.impl
      io2.IO.openFile.impl io2.IO.putBytes.impl io2.IO.ref
      io2.IO.removeDirectory.impl io2.IO.removeFile.impl
      io2.IO.renameDirectory.impl io2.IO.renameFile.impl
      io2.IO.seekHandle.impl io2.IO.serverSocket.impl
      io2.IO.setBuffering.impl io2.IO.setCurrentDirectory.impl
      io2.IO.socketAccept.impl io2.IO.socketPort.impl
      io2.IO.socketReceive.impl io2.IO.socketSend.impl
      io2.IO.stdHandle io2.IO.systemTime.impl
      io2.IO.systemTimeMicroseconds io2.IOError
      io2.IOError.AlreadyExists io2.IOError.EOF
      io2.IOError.IllegalOperation io2.IOError.NoSuchThing
      io2.IOError.PermissionDenied io2.IOError.ResourceBusy
      io2.IOError.ResourceExhausted io2.IOError.UserError
      io2.IOFailure io2.MVar io2.MVar.isEmpty io2.MVar.new
      io2.MVar.newEmpty io2.MVar.put.impl io2.MVar.read.impl
      io2.MVar.swap.impl io2.MVar.take.impl io2.MVar.tryPut.impl
      io2.MVar.tryRead.impl io2.MVar.tryTake io2.SeekMode
      io2.SeekMode.AbsoluteSeek io2.SeekMode.RelativeSeek
      io2.SeekMode.SeekFromEnd io2.Socket io2.StdHandle
      io2.StdHandle.StdErr io2.StdHandle.StdIn
      io2.StdHandle.StdOut io2.STM io2.STM.atomically
      io2.STM.retry io2.ThreadId io2.Tls io2.Tls.Cipher
      io2.Tls.ClientConfig io2.Tls.ClientConfig.certificates.set
      io2.TLS.ClientConfig.ciphers.set
      io2.Tls.ClientConfig.default
      io2.Tls.ClientConfig.versions.set io2.Tls.decodeCert.impl
      io2.Tls.decodePrivateKey io2.Tls.encodeCert
      io2.Tls.encodePrivateKey io2.Tls.handshake.impl
      io2.Tls.newClient.impl io2.Tls.newServer.impl
      io2.Tls.PrivateKey io2.Tls.receive.impl io2.Tls.send.impl
      io2.Tls.ServerConfig io2.Tls.ServerConfig.certificates.set
      io2.Tls.ServerConfig.ciphers.set
      io2.Tls.ServerConfig.default
      io2.Tls.ServerConfig.versions.set io2.Tls.SignedCert
      io2.Tls.terminate.impl io2.Tls.Version io2.TlsFailure
      io2.TVar io2.TVar.new io2.TVar.newIO io2.TVar.read
      io2.TVar.readIO io2.TVar.swap io2.TVar.write IsPropagated
      IsPropagated.IsPropagated IsTest IsTest.IsTest Link
      Link.Term.toText List List.++ List.+: List.:+ List.at
      List.cons List.drop List.empty List.size List.snoc
      List.take metadata.isPropagated metadata.isTest Nat Nat.*
      Nat.+ Nat./ Nat.and Nat.complement Nat.drop Nat.eq
      Nat.fromText Nat.gt Nat.gteq Nat.increment Nat.isEven
      Nat.isOdd Nat.leadingZeros Nat.lt Nat.lteq Nat.mod Nat.or
      Nat.popCount Nat.pow Nat.shiftLeft Nat.shiftRight Nat.sub
      Nat.toFloat Nat.toInt Nat.toText Nat.trailingZeros Nat.xor
      Optional Optional.None Optional.Some Ref Ref.read
      Ref.write Request Scope Scope.ref Scope.run SeqView
      SeqView.VElem SeqView.VEmpty Test.Result Test.Result.Fail
      Test.Result.Ok Text Text.!= Text.++ Text.drop Text.empty
      Text.eq Text.fromCharList Text.fromUtf8.impl Text.gt
      Text.gteq Text.lt Text.lteq Text.repeat Text.size
      Text.take Text.toCharList Text.toUtf8 Text.uncons
      Text.unsnoc todo Tuple Tuple.Cons Unit Unit.Unit
      Universal.< Universal.<= Universal.== Universal.>
      Universal.>= Universal.compare unsafe.coerceAbilities
      Value Value.dependencies Value.deserialize Value.load
      Value.serialize Value.value Link.Term##Link.Term
      Link.Term#quh#0 Link.Type##Link.Type Link.Type#quh#1
  
  □ #sjg2v58vn2 (start of history)

```
Notice that `Nat.+` and `Nat.*` are deleted by the squash, and we see them deleted in one atomic step in the history.

Just confirming that those two definitions are in fact removed:

```ucm
.delete> view .delete.builtin.Nat.+

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    .delete.builtin.Nat.+

```
```ucm
.delete> view .delete.builtin.Nat.*

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    .delete.builtin.Nat.*

```
## Caveats

If you `squash mystuff trunk`, you're discarding any history of `mystuff` and just cons'ing onto the history of `trunk`. Thus, don't expect to be able to `merge trunk mystuff later and get great results. Squashing should only be used when you don't care about the history (and you know others haven't pulled and built on your line of history being discarded, so they don't care about the history either).
