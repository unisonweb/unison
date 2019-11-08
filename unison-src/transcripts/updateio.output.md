# Updating a large data type in Unison

I was updating the `IO` data type in Unison, which is fairly large, and I wanted to share a transcript of what the Unison experience is currently when updating a data type with many constructor and dependents.

First let's pull the .base library from GitHub at the commit just before I made this change:

```ucm
.> pull git@github.com:unisonweb/base.git .base 7ec9cf12006d524dba7486af5540d4649ab07c06

  🆕
  
  Here's what's changed in .base after the pull:
  
  + Adds / updates:
  
    . <| andThen Boolean Boolean.not Bytes Bytes.++ Bytes.at
    Bytes.drop Bytes.empty Bytes.flatten Bytes.fromList
    Bytes.size Bytes.take Bytes.toList Char Char.fromNat
    Char.toNat const Debug.watch Either Either.Left Either.Right
    Float Float.* Float.+ Float.- Float./ Float.abs Float.acos
    Float.acosh Float.asin Float.asinh Float.atan Float.atan2
    Float.atanh Float.ceiling Float.cos Float.cosh Float.eq
    Float.exp Float.floor Float.fromText Float.gt Float.gteq
    Float.log Float.logBase Float.lt Float.lteq Float.max
    Float.min Float.pow Float.round Float.sin Float.sinh
    Float.sqrt Float.tan Float.tanh Float.toText Float.truncate
    Heap Heap.children Heap.fromKeys Heap.fromList Heap.Heap
    Heap.max Heap.maxKey Heap.pop Heap.singleton Heap.size
    Heap.sort Heap.sortDescending Heap.union id Int Int.* Int.+
    Int.- Int./ Int.eq Int.fromText Int.gt Int.gteq
    Int.increment Int.isEven Int.isOdd Int.lt Int.lteq
    Int.maxInt Int.minInt Int.mod Int.negate Int.signum
    Int.toFloat Int.toText Int.truncate0 io.accept io.bracket
    io.BufferMode io.BufferMode.Block io.BufferMode.Line
    io.clientSocket io.closeFile io.closeSocket
    io.createDirectory io.delay io.directoryContents
    io.EpochTime io.EpochTime.EpochTime io.Error io.Error.Error
    io.ErrorDescription io.ErrorDescription.ErrorDescription
    io.ErrorLocation io.ErrorLocation.ErrorLocation io.ErrorType
    io.ErrorType.AlreadyExists io.ErrorType.EOF
    io.ErrorType.IllegalOperation io.ErrorType.NoSuchThing
    io.ErrorType.PermissionDenied io.ErrorType.ResourceBusy
    io.ErrorType.ResourceExhausted io.ErrorType.UserError
    io.fileExists io.FilePath io.FilePath.FilePath io.fork
    io.getBuffering io.getCurrentDirectory io.getFileSize
    io.getFileTimestamp io.getLine io.getTemporaryDirectory
    io.getText io.Handle io.Handle.Handle io.HostName
    io.HostName.HostName io.IO io.IO.accept_ io.IO.bracket_
    io.IO.clientSocket_ io.IO.closeFile_ io.IO.closeSocket_
    io.IO.createDirectory_ io.IO.delay_ io.IO.directoryContents_
    io.IO.fileExists_ io.IO.fork_ io.IO.getBuffering_
    io.IO.getCurrentDirectory_ io.IO.getFileSize_
    io.IO.getFileTimestamp_ io.IO.getLine_
    io.IO.getTemporaryDirectory_ io.IO.getText_
    io.IO.isDirectory_ io.IO.isFileEOF_ io.IO.isFileOpen_
    io.IO.isSeekable_ io.IO.kill_ io.IO.listen_ io.IO.openFile_
    io.IO.position_ io.IO.putText_ io.IO.receive_
    io.IO.removeDirectory_ io.IO.removeFile_
    io.IO.renameDirectory_ io.IO.renameFile_ io.IO.seek_
    io.IO.send_ io.IO.serverSocket_ io.IO.setBuffering_
    io.IO.setCurrentDirectory_ io.IO.systemTime_ io.IO.throw
    io.isDirectory io.isFileEOF io.isFileOpen io.isSeekable
    io.kill io.listen io.Mode io.Mode.Append io.Mode.Read
    io.Mode.ReadWrite io.Mode.Write io.openFile io.position
    io.printLine io.putText io.readLine io.receive
    io.removeDirectory io.removeFile io.renameDirectory
    io.renameFile io.rethrow io.seek io.SeekMode
    io.SeekMode.Absolute io.SeekMode.FromEnd
    io.SeekMode.Relative io.send io.serverSocket io.ServiceName
    io.ServiceName.ServiceName io.setBuffering
    io.setCurrentDirectory io.Socket io.Socket.Socket io.stderr
    io.stdin io.stdout io.systemTime io.ThreadId
    io.ThreadId.ThreadId IsTest IsTest.IsTest links.isTest List
    List.++ List.+: List.:+ List.at List.cons List.diagonal
    List.distinct List.drop List.empty List.flatMap List.foldb
    List.foldl List.halve List.indexed List.insert List.join
    List.map List.range List.replace List.reverse List.size
    List.slice List.snoc List.sortBy List.take List.uncons
    List.unfold List.unsafeAt List.unsnoc List.zip Map
    Map.contains Map.empty Map.fromList Map.insert Map.intersect
    Map.intersectWith Map.keys Map.lookup Map.Map Map.map
    Map.mapKeys Map.singleton Map.size Map.toList Map.union
    Map.unionWith Map.values Multimap.insert Multimap.lookup Nat
    Nat.* Nat.+ Nat.- Nat./ Nat.drop Nat.eq Nat.fromText Nat.gt
    Nat.gteq Nat.increment Nat.isEven Nat.isOdd Nat.lt Nat.lteq
    Nat.maxNat Nat.mod Nat.sub Nat.toFloat Nat.toInt Nat.toText
    Optional Optional.flatMap Optional.map Optional.map2
    Optional.None Optional.orDefault Optional.orElse
    Optional.Some Request Search.exact Search.indexOf Search.lub
    Search.lubIndexOf Search.lubIndexOf' Set Set.contains
    Set.empty Set.fromList Set.insert Set.intersect Set.Set
    Set.size Set.toList Set.toMap Set.underlying Set.union
    test.internals.v1.Domain test.internals.v1.Domain.boolean
    test.internals.v1.Domain.ints test.internals.v1.Domain.Large
    test.internals.v1.Domain.lift2
    test.internals.v1.Domain.lists
    test.internals.v1.Domain.listsOf
    test.internals.v1.Domain.map test.internals.v1.Domain.nats
    test.internals.v1.Domain.pairs
    test.internals.v1.Domain.sample
    test.internals.v1.Domain.Small
    test.internals.v1.Domain.smallSize
    test.internals.v1.Domain.tuples
    test.internals.v1.Domain.weighted
    test.internals.v1.foldReport test.internals.v1.foldScope
    test.internals.v1.foldStatus test.internals.v1.foldSuccess
    test.internals.v1.Scope.cons
    test.internals.v1.Status.combine
    test.internals.v1.Status.pending
    test.internals.v1.Success.combine test.internals.v1.Test.&&
    test.internals.v1.Test.check test.internals.v1.Test.check'
    test.internals.v1.Test.failed
    test.internals.v1.Test.failedWith
    test.internals.v1.Test.finished
    test.internals.v1.Test.forAll test.internals.v1.Test.forAll'
    test.internals.v1.Test.label
    test.internals.v1.Test.modifyScope
    test.internals.v1.Test.modifyStatus
    test.internals.v1.Test.passed
    test.internals.v1.Test.passedUnexpectedly
    test.internals.v1.Test.passedWith
    test.internals.v1.Test.pending test.internals.v1.Test.proved
    test.internals.v1.Test.provedUnexpectedly
    test.internals.v1.Test.provedWith
    test.internals.v1.Test.Report test.internals.v1.Test.report
    test.internals.v1.Test.Report.combine
    test.internals.v1.Test.Report.empty
    test.internals.v1.Test.Report.Report
    test.internals.v1.Test.Report.toCLIResult
    test.internals.v1.Test.run test.internals.v1.Test.runAll
    test.internals.v1.Test.Scope
    test.internals.v1.Test.Scope.Scope
    test.internals.v1.Test.Status
    test.internals.v1.Test.Status.Expected
    test.internals.v1.Test.Status.Failed
    test.internals.v1.Test.Status.Pending
    test.internals.v1.Test.Status.Unexpected
    test.internals.v1.Test.Success
    test.internals.v1.Test.Success.Passed
    test.internals.v1.Test.Success.Proved
    test.internals.v1.Test.Test test.internals.v1.Test.Test.Test
    Test.Result Test.Result.Fail Test.Result.Ok test.v1.both
    test.v1.cost test.v1.empty test.v1.expect test.v1.fail
    test.v1.failWith test.v1.Gen test.v1.Gen.sample
    test.v1.Gen.toWeighted test.v1.label test.v1.list
    test.v1.nat test.v1.natIn test.v1.ok test.v1.okWith
    test.v1.pick test.v1.prove test.v1.proveWith test.v1.run
    test.v1.runs test.v1.sample test.v1.Test test.v1.tests
    test.v1.unexpected.ok test.v1.unexpected.proven Text Text.!=
    Text.++ Text.drop Text.empty Text.eq Text.fromCharList
    Text.gt Text.gteq Text.lt Text.lteq Text.size Text.take
    Text.toCharList Text.uncons Text.unsnoc Trie Trie.empty
    Trie.head Trie.head.modify Trie.head.set Trie.insert
    Trie.lookup Trie.map Trie.mapKeys Trie.singleton Trie.tail
    Trie.tail.modify Trie.tail.set Trie.Trie Trie.union
    Trie.unionWith Tuple Tuple.at1 Tuple.at2 Tuple.at3 Tuple.at4
    Tuple.Cons Unit Unit.Unit Universal.< Universal.<=
    Universal.== Universal.> Universal.>= Universal.compare
    Weighted Weighted.<|> Weighted.Fail Weighted.flatMap
    Weighted.fromList Weighted.lists Weighted.map
    Weighted.mergeWith Weighted.nats Weighted.sample
    Weighted.Weight Weighted.weight Weighted.Yield
    Weighted.yield |>
  
  Tip: You can always `undo` if this wasn't what you wanted.

.> fork .base.io runar.work.io

  Done.

```
The thing I wanted to do was change the `io.Mode` type so that it's a `unique` type rather than a structured type. I just add the `unique` modifier to the type declaration:

```unison
unique type Mode = Read | Write | Append | ReadWrite
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Mode
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Then I tab to the Unison Codebase manager and `update`:

```ucm
.runar.work.io> update

  ⍟ I've updated to these definitions:
  
    unique type Mode

.runar.work.io> todo:error

```
Looks like there are 41 transitive dependents and the first thing to update is the `IO` type. Specifically its `openFile` constructor, and the corresponding smart constructor.

I then edit the `IO` type with `edit IO`. Everything in here has `.base.io.Mode` instead of my new `.runar.work.io.Mode` since this is the old `IO` type. I simply remove `.base.io` everywhere in the text.

```unison
ability IO where
  getFileSize_ : FilePath ->{IO} .base.Either Error .base.Nat
  kill_ : ThreadId ->{IO} .base.Either Error ()
  send_ : Socket -> .base.Bytes ->{IO} .base.Either Error ()
  bracket_ : '{IO} a -> (a ->{IO} b) -> (a ->{IO} c) ->{IO} .base.Either Error c
  getLine_ : Handle ->{IO} .base.Either Error .base.Text
  getText_ : Handle ->{IO} .base.Either Error .base.Text
  getFileTimestamp_ : FilePath ->{IO} .base.Either Error EpochTime
  closeFile_ : Handle ->{IO} .base.Either Error ()
  getTemporaryDirectory_ : {IO} (.base.Either Error FilePath)
  getCurrentDirectory_ : {IO} (.base.Either Error FilePath)
  renameDirectory_ : FilePath -> FilePath ->{IO} .base.Either Error ()
  renameFile_ : FilePath -> FilePath ->{IO} .base.Either Error ()
  receive_ : Socket -> .base.Nat ->{IO} .base.Either Error (.base.Optional .base.Bytes)
  fileExists_ : FilePath ->{IO} .base.Either Error .base.Boolean
  isDirectory_ : FilePath ->{IO} .base.Either Error .base.Boolean
  directoryContents_ : FilePath ->{IO} .base.Either Error [FilePath]
  listen_ : Socket ->{IO} .base.Either Error ()
  closeSocket_ : Socket ->{IO} .base.Either Error ()
  clientSocket_ : HostName -> ServiceName ->{IO} .base.Either Error Socket
  delay_ : .base.Nat ->{IO} .base.Either Error ()
  seek_ : Handle -> SeekMode -> .base.Int ->{IO} .base.Either Error ()
  serverSocket_ : .base.Optional HostName -> ServiceName ->{IO} .base.Either Error Socket
  accept_ : Socket ->{IO} .base.Either Error Socket
  setBuffering_ : Handle -> .base.Optional BufferMode ->{IO} .base.Either Error ()
  openFile_ : FilePath -> Mode ->{IO} .base.Either Error Handle
  throw : Error ->{IO} a
  fork_ : '{IO} a ->{IO} .base.Either Error ThreadId
  getBuffering_ : Handle ->{IO} .base.Either Error (.base.Optional BufferMode)
  position_ : Handle ->{IO} .base.Either Error .base.Int
  setCurrentDirectory_ : FilePath ->{IO} .base.Either Error ()
  createDirectory_ : FilePath ->{IO} .base.Either Error ()
  removeDirectory_ : FilePath ->{IO} .base.Either Error ()
  removeFile_ : FilePath ->{IO} .base.Either Error ()
  systemTime_ : {IO} (.base.Either Error EpochTime)
  isFileEOF_ : Handle ->{IO} .base.Either Error .base.Boolean
  isFileOpen_ : Handle ->{IO} .base.Either Error .base.Boolean
  isSeekable_ : Handle ->{IO} .base.Either Error .base.Boolean
  putText_ : Handle -> .base.Text ->{IO} .base.Either Error ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      ability IO
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
And `update`:

```ucm
.runar.work.io> update

  ⍟ I've updated to these definitions:
  
    ability IO

  🚧
  
  The namespace has 40 transitive dependent(s) left to upgrade.
  Your edit frontier is the dependents of these definitions:
  
    type .base.io.Mode
    ability .base.io.IO
  
  I recommend working on them in the following order:
  
  1.  renameFile            : FilePath
                              -> FilePath
                              ->{.base.io.IO} ()
  2.  createDirectory       : FilePath ->{.base.io.IO} ()
  3.  systemTime            : '{.base.io.IO} EpochTime
  4.  openFile              : FilePath
                              -> .base.io.Mode
                              ->{.base.io.IO} Handle
  5.  getLine               : Handle ->{.base.io.IO} .base.Text
  6.  bracket               : '{.base.io.IO} a
                              -> (a ->{.base.io.IO} b)
                              -> (a ->{.base.io.IO} c)
                              ->{.base.io.IO} c
  7.  send                  : Socket
                              -> .base.Bytes
                              ->{.base.io.IO} ()
  8.  removeFile            : FilePath ->{.base.io.IO} ()
  9.  delay                 : .base.Nat ->{.base.io.IO} ()
  10. isFileEOF             : Handle
                              ->{.base.io.IO} .base.Boolean
  11. removeDirectory       : FilePath ->{.base.io.IO} ()
  12. position              : Handle ->{.base.io.IO} .base.Int
  13. getCurrentDirectory   : '{.base.io.IO} FilePath
  14. setCurrentDirectory   : FilePath ->{.base.io.IO} ()
  15. directoryContents     : FilePath
                              ->{.base.io.IO} [FilePath]
  16. isFileOpen            : Handle
                              ->{.base.io.IO} .base.Boolean
  17. fileExists            : FilePath
                              ->{.base.io.IO} .base.Boolean
  18. getBuffering          : Handle
                              ->{.base.io.IO} .base.Optional
                                BufferMode
  19. readLine              : '{.base.io.IO} .base.Text
  20. getTemporaryDirectory : '{.base.io.IO} FilePath
  21. getFileTimestamp      : FilePath ->{.base.io.IO} EpochTime
  22. isDirectory           : FilePath
                              ->{.base.io.IO} .base.Boolean
  23. seek                  : Handle
                              -> SeekMode
                              -> .base.Int
                              ->{.base.io.IO} ()
  24. receive               : Socket
                              -> .base.Nat
                              ->{.base.io.IO} .base.Optional
                                .base.Bytes
  25. getText               : Handle ->{.base.io.IO} .base.Text
  26. setBuffering          : Handle
                              -> .base.Optional BufferMode
                              ->{.base.io.IO} ()
  27. getFileSize           : FilePath ->{.base.io.IO} .base.Nat
  28. serverSocket          : .base.Optional HostName
                              -> ServiceName
                              ->{.base.io.IO} Socket
  29. fork                  : '{.base.io.IO} a
                              ->{.base.io.IO} ThreadId
  30. closeFile             : Handle ->{.base.io.IO} ()
  31. rethrow               : .base.Either Error a
                              ->{.base.io.IO} a
  32. accept                : Socket ->{.base.io.IO} Socket
  33. clientSocket          : HostName
                              -> ServiceName
                              ->{.base.io.IO} Socket
  34. kill                  : ThreadId ->{.base.io.IO} ()
  35. closeSocket           : Socket ->{.base.io.IO} ()
  36. printLine             : .base.Text ->{.base.io.IO} ()
  37. listen                : Socket ->{.base.io.IO} ()
  38. renameDirectory       : FilePath
                              -> FilePath
                              ->{.base.io.IO} ()
  39. isSeekable            : Handle
                              ->{.base.io.IO} .base.Boolean
  40. putText               : Handle
                              -> .base.Text
                              ->{.base.io.IO} ()
  
  

```
Now I have 40 transitive dependents left to upgrade. The things I need to work on next (my "edit frontier") are all the derived `IO` operations and smart constructors.

Now, `edit 1-40` opens my entire edit frontier in my editor. At this point I simply remove `.base.io` from the whole file and save it.

```unison
accept : Socket ->{IO} Socket
accept s = rethrow (IO.accept_ s)

bracket :
  '{IO} a
  -> (a ->{IO} b)
  -> (a ->{IO} c)
  ->{IO} c
bracket acquire release what =
  rethrow (IO.bracket_ acquire release what)

clientSocket :
  HostName -> ServiceName ->{IO} Socket
clientSocket host service =
  rethrow (IO.clientSocket_ host service)

closeFile : Handle ->{IO} ()
closeFile f = rethrow (IO.closeFile_ f)

closeSocket : Socket ->{IO} ()
closeSocket s = rethrow (IO.closeSocket_ s)

createDirectory : FilePath ->{IO} ()
createDirectory d = rethrow (IO.createDirectory_ d)

delay : .base.Nat ->{IO} ()
delay n = rethrow (IO.delay_ n)

directoryContents : FilePath ->{IO} [FilePath]
directoryContents d =
  rethrow (IO.directoryContents_ d)

fileExists : FilePath ->{IO} .base.Boolean
fileExists d = rethrow (IO.fileExists_ d)

fork : '{IO} a ->{IO} ThreadId
fork a = rethrow (IO.fork_ a)

getBuffering :
  Handle ->{IO} .base.Optional BufferMode
getBuffering h = rethrow (IO.getBuffering_ h)

getCurrentDirectory : '{IO} FilePath
getCurrentDirectory _ =
  rethrow IO.getCurrentDirectory_

getFileSize : FilePath ->{IO} .base.Nat
getFileSize d = rethrow (IO.getFileSize_ d)

getFileTimestamp : FilePath ->{IO} EpochTime
getFileTimestamp d =
  rethrow (IO.getFileTimestamp_ d)

getLine : Handle ->{IO} .base.Text
getLine h = rethrow (IO.getLine_ h)

getTemporaryDirectory : '{IO} FilePath
getTemporaryDirectory _ =
  rethrow IO.getTemporaryDirectory_

getText : Handle ->{IO} .base.Text
getText h = rethrow (IO.getText_ h)

isDirectory : FilePath ->{IO} .base.Boolean
isDirectory d = rethrow (IO.isDirectory_ d)

isFileEOF : Handle ->{IO} .base.Boolean
isFileEOF h = rethrow (IO.isFileEOF_ h)

isFileOpen : Handle ->{IO} .base.Boolean
isFileOpen h = rethrow (IO.isFileOpen_ h)

isSeekable : Handle ->{IO} .base.Boolean
isSeekable h = rethrow (IO.isSeekable_ h)

kill : ThreadId ->{IO} ()
kill t = rethrow (IO.kill_ t)

listen : Socket ->{IO} ()
listen s = rethrow (IO.listen_ s)

openFile : FilePath -> Mode ->{IO} Handle
openFile f m = rethrow (IO.openFile_ f m)

position : Handle ->{IO} .base.Int
position h = rethrow (IO.position_ h)

printLine : .base.Text ->{IO} ()
printLine t =
  putText stdout t
  putText stdout "\n"

putText : Handle -> .base.Text ->{IO} ()
putText h t = rethrow (IO.putText_ h t)

readLine : '{IO} .base.Text
readLine _ = getLine stdin

receive :
  Socket
  -> .base.Nat
  ->{IO} .base.Optional .base.Bytes
receive s n = rethrow (IO.receive_ s n)

removeDirectory : FilePath ->{IO} ()
removeDirectory d = rethrow (IO.removeDirectory_ d)

removeFile : FilePath ->{IO} ()
removeFile d = rethrow (IO.removeFile_ d)

renameDirectory : FilePath -> FilePath ->{IO} ()
renameDirectory from to =
  rethrow (IO.renameDirectory_ from to)

renameFile : FilePath -> FilePath ->{IO} ()
renameFile from to =
  rethrow (IO.renameFile_ from to)

rethrow : .base.Either Error a ->{IO} a
rethrow x =
  case x of
    .base.Either.Left e -> IO.throw e
    .base.Either.Right a -> a

seek : Handle -> SeekMode -> .base.Int ->{IO} ()
seek h m i = rethrow (IO.seek_ h m i)

send : Socket -> .base.Bytes ->{IO} ()
send s bs = rethrow (IO.send_ s bs)

serverSocket :
  .base.Optional HostName
  -> ServiceName
  ->{IO} Socket
serverSocket host service =
  rethrow (IO.serverSocket_ host service)

setBuffering :
  Handle -> .base.Optional BufferMode ->{IO} ()
setBuffering h bm = rethrow (IO.setBuffering_ h bm)

setCurrentDirectory : FilePath ->{IO} ()
setCurrentDirectory d =
  rethrow (IO.setCurrentDirectory_ d)

systemTime : '{IO} EpochTime
systemTime _ = rethrow IO.systemTime_
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      accept                : Socket ->{IO} Socket
      bracket               : '{IO} a
                              -> (a ->{IO} b)
                              -> (a ->{IO} c)
                              ->{IO} c
      clientSocket          : HostName
                              -> ServiceName
                              ->{IO} Socket
      closeFile             : Handle ->{IO} ()
      closeSocket           : Socket ->{IO} ()
      createDirectory       : FilePath ->{IO} ()
      delay                 : .base.Nat ->{IO} ()
      directoryContents     : FilePath ->{IO} [FilePath]
      fileExists            : FilePath ->{IO} .base.Boolean
      fork                  : '{IO} a ->{IO} ThreadId
      getBuffering          : Handle
                              ->{IO} .base.Optional BufferMode
      getCurrentDirectory   : '{IO} FilePath
      getFileSize           : FilePath ->{IO} .base.Nat
      getFileTimestamp      : FilePath ->{IO} EpochTime
      getLine               : Handle ->{IO} .base.Text
      getTemporaryDirectory : '{IO} FilePath
      getText               : Handle ->{IO} .base.Text
      isDirectory           : FilePath ->{IO} .base.Boolean
      isFileEOF             : Handle ->{IO} .base.Boolean
      isFileOpen            : Handle ->{IO} .base.Boolean
      isSeekable            : Handle ->{IO} .base.Boolean
      kill                  : ThreadId ->{IO} ()
      listen                : Socket ->{IO} ()
      openFile              : FilePath -> Mode ->{IO} Handle
      position              : Handle ->{IO} .base.Int
      printLine             : .base.Text ->{IO} ()
      putText               : Handle -> .base.Text ->{IO} ()
      readLine              : '{IO} .base.Text
      receive               : Socket
                              -> .base.Nat
                              ->{IO} .base.Optional .base.Bytes
      removeDirectory       : FilePath ->{IO} ()
      removeFile            : FilePath ->{IO} ()
      renameDirectory       : FilePath -> FilePath ->{IO} ()
      renameFile            : FilePath -> FilePath ->{IO} ()
      rethrow               : .base.Either Error a ->{IO} a
      seek                  : Handle
                              -> SeekMode
                              -> .base.Int
                              ->{IO} ()
      send                  : Socket -> .base.Bytes ->{IO} ()
      serverSocket          : .base.Optional HostName
                              -> ServiceName
                              ->{IO} Socket
      setBuffering          : Handle
                              -> .base.Optional BufferMode
                              ->{IO} ()
      setCurrentDirectory   : FilePath ->{IO} ()
      systemTime            : '{IO} EpochTime
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Then I tab back to UCM and `update`:

```ucm
.runar.work.io> update

  ⍟ I've updated to these definitions:
  
    accept                : Socket ->{IO} Socket
    bracket               : '{IO} a
                            -> (a ->{IO} b)
                            -> (a ->{IO} c)
                            ->{IO} c
    clientSocket          : HostName
                            -> ServiceName
                            ->{IO} Socket
    closeFile             : Handle ->{IO} ()
    closeSocket           : Socket ->{IO} ()
    createDirectory       : FilePath ->{IO} ()
    delay                 : .base.Nat ->{IO} ()
    directoryContents     : FilePath ->{IO} [FilePath]
    fileExists            : FilePath ->{IO} .base.Boolean
    fork                  : '{IO} a ->{IO} ThreadId
    getBuffering          : Handle
                            ->{IO} .base.Optional BufferMode
    getCurrentDirectory   : '{IO} FilePath
    getFileSize           : FilePath ->{IO} .base.Nat
    getFileTimestamp      : FilePath ->{IO} EpochTime
    getLine               : Handle ->{IO} .base.Text
    getTemporaryDirectory : '{IO} FilePath
    getText               : Handle ->{IO} .base.Text
    isDirectory           : FilePath ->{IO} .base.Boolean
    isFileEOF             : Handle ->{IO} .base.Boolean
    isFileOpen            : Handle ->{IO} .base.Boolean
    isSeekable            : Handle ->{IO} .base.Boolean
    kill                  : ThreadId ->{IO} ()
    listen                : Socket ->{IO} ()
    openFile              : FilePath -> Mode ->{IO} Handle
    position              : Handle ->{IO} .base.Int
    printLine             : .base.Text ->{IO} ()
    putText               : Handle -> .base.Text ->{IO} ()
    readLine              : '{IO} .base.Text
    receive               : Socket
                            -> .base.Nat
                            ->{IO} .base.Optional .base.Bytes
    removeDirectory       : FilePath ->{IO} ()
    removeFile            : FilePath ->{IO} ()
    renameDirectory       : FilePath -> FilePath ->{IO} ()
    renameFile            : FilePath -> FilePath ->{IO} ()
    rethrow               : .base.Either Error a ->{IO} a
    seek                  : Handle
                            -> SeekMode
                            -> .base.Int
                            ->{IO} ()
    send                  : Socket -> .base.Bytes ->{IO} ()
    serverSocket          : .base.Optional HostName
                            -> ServiceName
                            ->{IO} Socket
    setBuffering          : Handle
                            -> .base.Optional BufferMode
                            ->{IO} ()
    setCurrentDirectory   : FilePath ->{IO} ()
    systemTime            : '{IO} EpochTime

  ✅
  
  No conflicts or edits in progress.

```
And we're done! Unison made the update to `IO`, and also created a patch which can be applied to upgrade any code that uses `IO`:

```ucm
.runar.work.io> view.patch

  Edited Types:
    .base.io.Mode -> Mode
    .base.io.IO   -> IO
  
  Edited Terms:
    .base.io.renameFile            -> renameFile
    .base.io.createDirectory       -> createDirectory
    .base.io.systemTime            -> systemTime
    .base.io.openFile              -> openFile
    .base.io.getLine               -> getLine
    .base.io.bracket               -> bracket
    .base.io.send                  -> send
    .base.io.removeFile            -> removeFile
    .base.io.delay                 -> delay
    .base.io.isFileEOF             -> isFileEOF
    .base.io.removeDirectory       -> removeDirectory
    .base.io.position              -> position
    .base.io.getCurrentDirectory   -> getCurrentDirectory
    .base.io.setCurrentDirectory   -> setCurrentDirectory
    .base.io.directoryContents     -> directoryContents
    .base.io.isFileOpen            -> isFileOpen
    .base.io.fileExists            -> fileExists
    .base.io.getBuffering          -> getBuffering
    .base.io.readLine              -> readLine
    .base.io.getTemporaryDirectory -> getTemporaryDirectory
    .base.io.getFileTimestamp      -> getFileTimestamp
    .base.io.isDirectory           -> isDirectory
    .base.io.seek                  -> seek
    .base.io.receive               -> receive
    .base.io.getText               -> getText
    .base.io.setBuffering          -> setBuffering
    .base.io.getFileSize           -> getFileSize
    .base.io.serverSocket          -> serverSocket
    .base.io.fork                  -> fork
    .base.io.closeFile             -> closeFile
    .base.io.rethrow               -> rethrow
    .base.io.accept                -> accept
    .base.io.clientSocket          -> clientSocket
    .base.io.kill                  -> kill
    .base.io.closeSocket           -> closeSocket
    .base.io.printLine             -> printLine
    .base.io.listen                -> listen
    .base.io.renameDirectory       -> renameDirectory
    .base.io.isSeekable            -> isSeekable
    .base.io.putText               -> putText

```
