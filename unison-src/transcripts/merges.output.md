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
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

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
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

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
  
  ⊙ #be8v31d0lc
  
    - Deletes:
    
      feature1.y
  
  ⊙ #qcumplt3un
  
    + Adds / updates:
    
      master.y
    
    = Copies:
    
      Original name New name(s)
      feature1.y    master.y
  
  ⊙ #qf8ku0e8ja
  
    + Adds / updates:
    
      feature1.y
  
  ⊙ #jdd6dqbg1f
  
    > Moves:
    
      Original name New name
      x             master.x
  
  ⊙ #j222ocpe39
  
    + Adds / updates:
    
      x
  
  ⊙ #i5f8gcfb14
  
    + Adds / updates:
    
      builtin.Author builtin.Author.Author builtin.Author.guid
      builtin.Author.guid.modify builtin.Author.guid.set
      builtin.Author.name builtin.Author.name.modify
      builtin.Author.name.set builtin.Boolean
      builtin.Boolean.not builtin.Bytes builtin.Bytes.++
      builtin.Bytes.at builtin.Bytes.drop builtin.Bytes.empty
      builtin.Bytes.flatten builtin.Bytes.fromList
      builtin.Bytes.size builtin.Bytes.take builtin.Bytes.toList
      builtin.Char builtin.Char.fromNat builtin.Char.toNat
      builtin.CopyrightHolder
      builtin.CopyrightHolder.CopyrightHolder
      builtin.CopyrightHolder.guid
      builtin.CopyrightHolder.guid.modify
      builtin.CopyrightHolder.guid.set
      builtin.CopyrightHolder.name
      builtin.CopyrightHolder.name.modify
      builtin.CopyrightHolder.name.set builtin.Debug.watch
      builtin.Doc builtin.Doc.++ builtin.Doc.Blob
      builtin.Doc.Evaluate builtin.Doc.Join builtin.Doc.Link
      builtin.Doc.Signature builtin.Doc.Source builtin.Either
      builtin.Either.Left builtin.Either.Right builtin.Float
      builtin.Float.* builtin.Float.+ builtin.Float.-
      builtin.Float./ builtin.Float.abs builtin.Float.acos
      builtin.Float.acosh builtin.Float.asin builtin.Float.asinh
      builtin.Float.atan builtin.Float.atan2 builtin.Float.atanh
      builtin.Float.ceiling builtin.Float.cos builtin.Float.cosh
      builtin.Float.eq builtin.Float.exp builtin.Float.floor
      builtin.Float.fromText builtin.Float.gt builtin.Float.gteq
      builtin.Float.log builtin.Float.logBase builtin.Float.lt
      builtin.Float.lteq builtin.Float.max builtin.Float.min
      builtin.Float.pow builtin.Float.round builtin.Float.sin
      builtin.Float.sinh builtin.Float.sqrt builtin.Float.tan
      builtin.Float.tanh builtin.Float.toText
      builtin.Float.truncate builtin.GUID builtin.GUID.GUID
      builtin.Int builtin.Int.* builtin.Int.+ builtin.Int.-
      builtin.Int./ builtin.Int.eq builtin.Int.fromText
      builtin.Int.gt builtin.Int.gteq builtin.Int.increment
      builtin.Int.isEven builtin.Int.isOdd builtin.Int.lt
      builtin.Int.lteq builtin.Int.mod builtin.Int.negate
      builtin.Int.signum builtin.Int.toFloat builtin.Int.toText
      builtin.Int.truncate0 builtin.IsPropagated
      builtin.IsPropagated.IsPropagated builtin.IsTest
      builtin.IsTest.IsTest builtin.License
      builtin.License.License builtin.License.copyrightHolders
      builtin.License.copyrightHolders.modify
      builtin.License.copyrightHolders.set
      builtin.License.licenseType
      builtin.License.licenseType.modify
      builtin.License.licenseType.set builtin.License.years
      builtin.License.years.modify builtin.License.years.set
      builtin.LicenseType builtin.LicenseType.LicenseType
      builtin.Link builtin.Link.Term##Link.Term
      builtin.Link.Term#quh#0 builtin.Link.Type##Link.Type
      builtin.Link.Type#quh#1 builtin.List builtin.List.++
      builtin.List.+: builtin.List.:+ builtin.List.at
      builtin.List.cons builtin.List.drop builtin.List.empty
      builtin.List.size builtin.List.snoc builtin.List.take
      builtin.Nat builtin.Nat.* builtin.Nat.+ builtin.Nat./
      builtin.Nat.drop builtin.Nat.eq builtin.Nat.fromText
      builtin.Nat.gt builtin.Nat.gteq builtin.Nat.increment
      builtin.Nat.isEven builtin.Nat.isOdd builtin.Nat.lt
      builtin.Nat.lteq builtin.Nat.mod builtin.Nat.sub
      builtin.Nat.toFloat builtin.Nat.toInt builtin.Nat.toText
      builtin.Optional builtin.Optional.None
      builtin.Optional.Some builtin.Request builtin.Test.Result
      builtin.Test.Result.Fail builtin.Test.Result.Ok
      builtin.Text builtin.Text.!= builtin.Text.++
      builtin.Text.drop builtin.Text.empty builtin.Text.eq
      builtin.Text.fromCharList builtin.Text.gt
      builtin.Text.gteq builtin.Text.lt builtin.Text.lteq
      builtin.Text.size builtin.Text.take
      builtin.Text.toCharList builtin.Text.uncons
      builtin.Text.unsnoc builtin.Tuple builtin.Tuple.Cons
      builtin.Unit builtin.Unit.Unit builtin.Universal.<
      builtin.Universal.<= builtin.Universal.==
      builtin.Universal.> builtin.Universal.>=
      builtin.Universal.compare builtin.Year builtin.Year.Year
      builtin.bug builtin.io.BufferMode
      builtin.io.BufferMode.Block builtin.io.BufferMode.Line
      builtin.io.EpochTime builtin.io.EpochTime.EpochTime
      builtin.io.Error builtin.io.Error.Error
      builtin.io.ErrorDescription
      builtin.io.ErrorDescription.ErrorDescription
      builtin.io.ErrorLocation
      builtin.io.ErrorLocation.ErrorLocation
      builtin.io.ErrorType builtin.io.ErrorType.AlreadyExists
      builtin.io.ErrorType.EOF
      builtin.io.ErrorType.IllegalOperation
      builtin.io.ErrorType.NoSuchThing
      builtin.io.ErrorType.PermissionDenied
      builtin.io.ErrorType.ResourceBusy
      builtin.io.ErrorType.ResourceExhausted
      builtin.io.ErrorType.UserError builtin.io.FilePath
      builtin.io.FilePath.FilePath builtin.io.Handle
      builtin.io.Handle.Handle builtin.io.HostName
      builtin.io.HostName.HostName builtin.io.IO
      builtin.io.IO.accept_ builtin.io.IO.bracket_
      builtin.io.IO.clientSocket_ builtin.io.IO.closeFile_
      builtin.io.IO.closeSocket_ builtin.io.IO.createDirectory_
      builtin.io.IO.delay_ builtin.io.IO.directoryContents_
      builtin.io.IO.fileExists_ builtin.io.IO.fork_
      builtin.io.IO.getBuffering_
      builtin.io.IO.getCurrentDirectory_
      builtin.io.IO.getFileSize_ builtin.io.IO.getFileTimestamp_
      builtin.io.IO.getLine_
      builtin.io.IO.getTemporaryDirectory_
      builtin.io.IO.getText_ builtin.io.IO.isDirectory_
      builtin.io.IO.isFileEOF_ builtin.io.IO.isFileOpen_
      builtin.io.IO.isSeekable_ builtin.io.IO.kill_
      builtin.io.IO.listen_ builtin.io.IO.openFile_
      builtin.io.IO.position_ builtin.io.IO.putText_
      builtin.io.IO.receive_ builtin.io.IO.removeDirectory_
      builtin.io.IO.removeFile_ builtin.io.IO.renameDirectory_
      builtin.io.IO.renameFile_ builtin.io.IO.seek_
      builtin.io.IO.send_ builtin.io.IO.serverSocket_
      builtin.io.IO.setBuffering_
      builtin.io.IO.setCurrentDirectory_
      builtin.io.IO.systemTime_ builtin.io.IO.throw
      builtin.io.Mode builtin.io.Mode.Append
      builtin.io.Mode.Read builtin.io.Mode.ReadWrite
      builtin.io.Mode.Write builtin.io.SeekMode
      builtin.io.SeekMode.Absolute builtin.io.SeekMode.FromEnd
      builtin.io.SeekMode.Relative builtin.io.ServiceName
      builtin.io.ServiceName.ServiceName builtin.io.Socket
      builtin.io.Socket.Socket builtin.io.ThreadId
      builtin.io.ThreadId.ThreadId builtin.io.accept
      builtin.io.bracket builtin.io.clientSocket
      builtin.io.closeFile builtin.io.closeSocket
      builtin.io.createDirectory builtin.io.delay
      builtin.io.directoryContents builtin.io.fileExists
      builtin.io.fork builtin.io.getBuffering
      builtin.io.getCurrentDirectory builtin.io.getFileSize
      builtin.io.getFileTimestamp builtin.io.getLine
      builtin.io.getTemporaryDirectory builtin.io.getText
      builtin.io.isDirectory builtin.io.isFileEOF
      builtin.io.isFileOpen builtin.io.isSeekable
      builtin.io.kill builtin.io.listen builtin.io.openFile
      builtin.io.position builtin.io.printLine
      builtin.io.putText builtin.io.readLine builtin.io.receive
      builtin.io.removeDirectory builtin.io.removeFile
      builtin.io.renameDirectory builtin.io.renameFile
      builtin.io.rethrow builtin.io.seek builtin.io.send
      builtin.io.serverSocket builtin.io.setBuffering
      builtin.io.setCurrentDirectory builtin.io.stderr
      builtin.io.stdin builtin.io.stdout builtin.io.systemTime
      builtin.metadata.isPropagated builtin.metadata.isTest
      builtin.todo
  
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
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

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
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> update

  ⍟ I've added these definitions:
  
    master.frobnicate : builtin.Nat -> builtin.Nat
  
  ⍟ I've updated these names to your new definition:
  
    master.y : builtin.Text
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
