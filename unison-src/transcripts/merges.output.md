# Forking and merging namespaces in `ucm`

The Unison namespace is a versioned tree of names that map to Unison definitions. You can change this namespace and fork and merge subtrees of it. Let's start by introducing a few definitions into a new namespace, `foo`:

```unison
x = 42
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat

```
Let's move `x` into a new namespace, `master`:

```ucm
.> rename.term x master.x

  Done.

```
If you want to do some experimental work in a namespace without disturbing anyone else, you can `fork` it (which is a shorthand for `copy.namespace`). This creates a copy of it, preserving its history.

> __Note:__ these copies are very efficient to create as they just have pointers into the same underlying definitions. Create as many as you like.

Let's go ahead and do this:

```
.> fork master feature1
.> view master.x
.> view feature1.x

```

Great! We can now do some further work in the `feature1` branch, then merge it back into `master` when we're ready.

```unison
y = "hello"
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      y : Text

```
```ucm
  ☝️  The namespace .feature1 is empty.

.feature1> add

  ⍟ I've added these definitions:
  
    y : Text

.master> merge .feature1

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. y : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.master> view y

  y : Text
  y = "hello"

```
> Note: `merge src`, with one argument, merges `src` into the current namespace. You can also do `merge src dest` to merge into any destination namespace.

Notice that `master` now has the definition of `y` we wrote.

We can also delete the fork if we're done with it. (Don't worry, it's still in the `history` and can be resurrected at any time.)

```ucm
.> delete.namespace .feature1

  Removed definitions:
  
    1. y : Text
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #suq47on34k
  
    - Deletes:
    
      feature1.y
  
  ⊙ #trcl8lj5r6
  
    + Adds / updates:
    
      master.y
    
    = Copies:
    
      Original name New name(s)
      feature1.y    master.y
  
  ⊙ #oddqh6udcc
  
    + Adds / updates:
    
      feature1.y
  
  ⊙ #3ro66pat9g
  
    > Moves:
    
      Original name New name
      x             master.x
  
  ⊙ #ggpl9bg0eh
  
    + Adds / updates:
    
      x
  
  ⊙ #ilh91mocfb
  
    + Adds / updates:
    
      builtin.Any builtin.Any.Any builtin.Boolean
      builtin.Boolean.not builtin.Bytes builtin.Bytes.++
      builtin.Bytes.at builtin.Bytes.drop builtin.Bytes.empty
      builtin.Bytes.flatten builtin.Bytes.fromBase16
      builtin.Bytes.fromBase32 builtin.Bytes.fromBase64
      builtin.Bytes.fromBase64UrlUnpadded builtin.Bytes.fromList
      builtin.Bytes.size builtin.Bytes.take
      builtin.Bytes.toBase16 builtin.Bytes.toBase32
      builtin.Bytes.toBase64 builtin.Bytes.toBase64UrlUnpadded
      builtin.Bytes.toList builtin.Char builtin.Char.fromNat
      builtin.Char.toNat builtin.Code builtin.Code.cache_
      builtin.Code.dependencies builtin.Code.deserialize
      builtin.Code.isMissing builtin.Code.lookup
      builtin.Code.serialize builtin.Debug.watch builtin.Doc
      builtin.Doc.Blob builtin.Doc.Evaluate builtin.Doc.Join
      builtin.Doc.Link builtin.Doc.Signature builtin.Doc.Source
      builtin.Either builtin.Either.Left builtin.Either.Right
      builtin.Float builtin.Float.* builtin.Float.+
      builtin.Float.- builtin.Float./ builtin.Float.abs
      builtin.Float.acos builtin.Float.acosh builtin.Float.asin
      builtin.Float.asinh builtin.Float.atan builtin.Float.atan2
      builtin.Float.atanh builtin.Float.ceiling
      builtin.Float.cos builtin.Float.cosh builtin.Float.eq
      builtin.Float.exp builtin.Float.floor
      builtin.Float.fromText builtin.Float.gt builtin.Float.gteq
      builtin.Float.log builtin.Float.logBase builtin.Float.lt
      builtin.Float.lteq builtin.Float.max builtin.Float.min
      builtin.Float.pow builtin.Float.round builtin.Float.sin
      builtin.Float.sinh builtin.Float.sqrt builtin.Float.tan
      builtin.Float.tanh builtin.Float.toText
      builtin.Float.truncate builtin.Int builtin.Int.*
      builtin.Int.+ builtin.Int.- builtin.Int./ builtin.Int.and
      builtin.Int.complement builtin.Int.eq builtin.Int.fromText
      builtin.Int.gt builtin.Int.gteq builtin.Int.increment
      builtin.Int.isEven builtin.Int.isOdd
      builtin.Int.leadingZeros builtin.Int.lt builtin.Int.lteq
      builtin.Int.mod builtin.Int.negate builtin.Int.or
      builtin.Int.popCount builtin.Int.pow builtin.Int.shiftLeft
      builtin.Int.shiftRight builtin.Int.signum
      builtin.Int.toFloat builtin.Int.toText
      builtin.Int.trailingZeros builtin.Int.truncate0
      builtin.Int.xor builtin.Link builtin.Link.Term##Link.Term
      builtin.Link.Term#quh#0 builtin.Link.Type##Link.Type
      builtin.Link.Type#quh#1 builtin.List builtin.List.++
      builtin.List.+: builtin.List.:+ builtin.List.at
      builtin.List.cons builtin.List.drop builtin.List.empty
      builtin.List.size builtin.List.snoc builtin.List.take
      builtin.Nat builtin.Nat.* builtin.Nat.+ builtin.Nat./
      builtin.Nat.and builtin.Nat.complement builtin.Nat.drop
      builtin.Nat.eq builtin.Nat.fromText builtin.Nat.gt
      builtin.Nat.gteq builtin.Nat.increment builtin.Nat.isEven
      builtin.Nat.isOdd builtin.Nat.leadingZeros builtin.Nat.lt
      builtin.Nat.lteq builtin.Nat.mod builtin.Nat.or
      builtin.Nat.popCount builtin.Nat.pow builtin.Nat.shiftLeft
      builtin.Nat.shiftRight builtin.Nat.sub builtin.Nat.toFloat
      builtin.Nat.toInt builtin.Nat.toText
      builtin.Nat.trailingZeros builtin.Nat.xor builtin.Optional
      builtin.Optional.None builtin.Optional.Some
      builtin.Request builtin.SeqView builtin.SeqView.VElem
      builtin.SeqView.VEmpty builtin.Test.Result
      builtin.Test.Result.Fail builtin.Test.Result.Ok
      builtin.Text builtin.Text.!= builtin.Text.++
      builtin.Text.drop builtin.Text.empty builtin.Text.eq
      builtin.Text.fromCharList builtin.Text.fromUtf8
      builtin.Text.gt builtin.Text.gteq builtin.Text.lt
      builtin.Text.lteq builtin.Text.size builtin.Text.take
      builtin.Text.toCharList builtin.Text.toUtf8
      builtin.Text.uncons builtin.Text.unsnoc builtin.Tuple
      builtin.Tuple.Cons builtin.Unit builtin.Unit.Unit
      builtin.Universal.< builtin.Universal.<=
      builtin.Universal.== builtin.Universal.>
      builtin.Universal.>= builtin.Universal.compare
      builtin.Value builtin.Value.dependencies
      builtin.Value.deserialize builtin.Value.load
      builtin.Value.serialize builtin.Value.value builtin.bug
      builtin.crypto.HashAlgorithm
      builtin.crypto.HashAlgorithm.Blake2b_256
      builtin.crypto.HashAlgorithm.Blake2b_512
      builtin.crypto.HashAlgorithm.Blake2s_256
      builtin.crypto.HashAlgorithm.Sha2_256
      builtin.crypto.HashAlgorithm.Sha2_512
      builtin.crypto.HashAlgorithm.Sha3_256
      builtin.crypto.HashAlgorithm.Sha3_512 builtin.crypto.hash
      builtin.crypto.hashBytes builtin.crypto.hmac
      builtin.crypto.hmacBytes builtin.io2.BufferMode
      builtin.io2.BufferMode.BlockBuffering
      builtin.io2.BufferMode.LineBuffering
      builtin.io2.BufferMode.NoBuffering
      builtin.io2.BufferMode.SizedBlockBuffering
      builtin.io2.Failure builtin.io2.Failure.Failure
      builtin.io2.FileMode builtin.io2.FileMode.Append
      builtin.io2.FileMode.Read builtin.io2.FileMode.ReadWrite
      builtin.io2.FileMode.Write builtin.io2.Handle
      builtin.io2.IO builtin.io2.IO.clientSocket
      builtin.io2.IO.closeFile builtin.io2.IO.closeSocket
      builtin.io2.IO.createDirectory
      builtin.io2.IO.createTempDirectory builtin.io2.IO.delay
      builtin.io2.IO.fileExists builtin.io2.IO.forkComp
      builtin.io2.IO.getBuffering builtin.io2.IO.getBytes
      builtin.io2.IO.getCurrentDirectory
      builtin.io2.IO.getFileSize builtin.io2.IO.getFileTimestamp
      builtin.io2.IO.getTempDirectory
      builtin.io2.IO.handlePosition builtin.io2.IO.isDirectory
      builtin.io2.IO.isFileEOF builtin.io2.IO.isFileOpen
      builtin.io2.IO.isSeekable builtin.io2.IO.kill
      builtin.io2.IO.listen builtin.io2.IO.openFile
      builtin.io2.IO.putBytes builtin.io2.IO.removeDirectory
      builtin.io2.IO.removeFile builtin.io2.IO.renameDirectory
      builtin.io2.IO.renameFile builtin.io2.IO.seekHandle
      builtin.io2.IO.serverSocket builtin.io2.IO.setBuffering
      builtin.io2.IO.setCurrentDirectory
      builtin.io2.IO.socketAccept builtin.io2.IO.socketPort
      builtin.io2.IO.socketReceive builtin.io2.IO.socketSend
      builtin.io2.IO.stdHandle builtin.io2.IO.systemTime
      builtin.io2.IOError builtin.io2.IOError.AlreadyExists
      builtin.io2.IOError.EOF
      builtin.io2.IOError.IllegalOperation
      builtin.io2.IOError.NoSuchThing
      builtin.io2.IOError.PermissionDenied
      builtin.io2.IOError.ResourceBusy
      builtin.io2.IOError.ResourceExhausted
      builtin.io2.IOError.UserError builtin.io2.MVar
      builtin.io2.MVar.isEmpty builtin.io2.MVar.new
      builtin.io2.MVar.newEmpty builtin.io2.MVar.put
      builtin.io2.MVar.read builtin.io2.MVar.swap
      builtin.io2.MVar.take builtin.io2.MVar.tryPut
      builtin.io2.MVar.tryRead builtin.io2.MVar.tryTake
      builtin.io2.SeekMode builtin.io2.SeekMode.AbsoluteSeek
      builtin.io2.SeekMode.RelativeSeek
      builtin.io2.SeekMode.SeekFromEnd builtin.io2.Socket
      builtin.io2.StdHandle builtin.io2.StdHandle.StdErr
      builtin.io2.StdHandle.StdIn builtin.io2.StdHandle.StdOut
      builtin.io2.ThreadId builtin.io2.Tls
      builtin.io2.Tls.ClientConfig
      builtin.io2.Tls.ClientConfig.certificates.set
      builtin.io2.Tls.ClientConfig.default
      builtin.io2.Tls.PrivateKey builtin.io2.Tls.ServerConfig
      builtin.io2.Tls.ServerConfig.certificates.set
      builtin.io2.Tls.ServerConfig.default
      builtin.io2.Tls.SignedCert builtin.io2.Tls.decodeCert
      builtin.io2.Tls.decodePrivateKey
      builtin.io2.Tls.encodeCert
      builtin.io2.Tls.encodePrivateKey builtin.io2.Tls.handshake
      builtin.io2.Tls.newClient builtin.io2.Tls.newServer
      builtin.io2.Tls.receive builtin.io2.Tls.send
      builtin.io2.Tls.terminate builtin.io2.TlsFailure
      builtin.io2.tls.ClientConfig.ciphers.set
      builtin.io2.tls.ClientConfig.versions.set
      builtin.io2.tls.ServerConfig.ciphers.set
      builtin.io2.tls.ServerConfig.versions.set builtin.todo
  
  □ #7asfbtqmoj (start of history)

```
To resurrect an old version of a namespace, you can learn its hash via the `history` command, then use `fork #namespacehash .newname`.

## Concurrent edits and merges

In the above scenario the destination namespace (`master`) was strictly behind the source namespace, so the merge didn't have anything interesting to do (Git would call this a "fast forward" merge). In other cases, the source and destination namespaces will each have changes the other doesn't know about, and the merge needs to something more interesting. That's okay too, and Unison will merge those results, using a 3-way merge algorithm.

> __Note:__ When merging nested namespaces, Unison actually uses a recursive 3-way merge, so it finds a different (and possibly closer) common ancestor at each level of the tree.

Let's see how this works. We are going to create a copy of `master`, add and delete some definitions in `master` and in the fork, then merge.

```ucm
.> fork master feature2

  Done.

```
Here's one fork, we add `z` and delete `x`:

```unison
z = 99
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      z : Nat

```
```ucm
.feature2> add

  ⍟ I've added these definitions:
  
    z : Nat

.feature2> delete.term x

  Name changes:
  
    Original         Changes
    1. feature2.x ┐  2. feature2.x (removed)
    3. master.x   ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
And here's the other fork, where we update `y` and add a new definition, `frobnicate`:

```unison
master.y = "updated y"
master.frobnicate n = n + 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      master.frobnicate : Nat -> Nat
      master.y          : Text

```
```ucm
.> update

  ⍟ I've added these definitions:
  
    master.frobnicate : Nat -> Nat
  
  ⍟ I've updated these names to your new definition:
  
    master.y : Text
      (The old definition was also named feature2.y. I updated
      this name too.)

.> view master.y

  feature2.y : Text
  feature2.y = "updated y"

.> view master.frobnicate

  master.frobnicate : Nat -> Nat
  master.frobnicate n =
    use Nat +
    n + 1

```
At this point, `master` and `feature2` both have some changes the other doesn't know about. Let's merge them.

```ucm
.> merge feature2 master

  Here's what's changed in master after the merge:
  
  Added definitions:
  
    1. z : Nat
  
  Removed definitions:
  
    2. x : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
Notice that `x` is deleted in the merged branch (it was deleted in `feature2` and untouched by `master`):

```ucm
.> view master.x

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    master.x

```
And notice that `y` has the most recent value, and that `z` and `frobnicate` both exist as well:

```ucm
.> view master.y

  feature2.y : Text
  feature2.y = "updated y"

.> view master.z

  feature2.z : Nat
  feature2.z = 99

.> view master.frobnicate

  master.frobnicate : Nat -> Nat
  master.frobnicate n =
    use Nat +
    n + 1

```
## FAQ

* What happens if namespace1 deletes a name that namespace2 has updated? A: ???
* ...
