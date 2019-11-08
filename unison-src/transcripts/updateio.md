# Updating a large data type in Unison

I was updating the `IO` data type in Unison, which is fairly large, and I wanted to share a transcript of what the Unison experience is currently when updating a data type with many constructor and dependents.

First let's pull the .base library from GitHub at the commit just before I made this change:

```ucm
.> pull git@github.com:unisonweb/base.git .base 7ec9cf12006d524dba7486af5540d4649ab07c06
.> fork .base.io runar.work.io
```

The thing I wanted to do was change the `io.Mode` type so that it's a `unique` type rather than a structured type. I just add the `unique` modifier to the type declaration:

```unison
unique type Mode = Read | Write | Append | ReadWrite
```

Then I tab to the Unison Codebase manager and `update`:

```ucm
.runar.work.io> update
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

And `update`:

```ucm
.runar.work.io> update
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

Then I tab back to UCM and `update`:

```ucm
.runar.work.io> update
```

And we're done! Unison made the update to `IO`, and also created a patch which can be applied to upgrade any code that uses `IO`:

```ucm
.runar.work.io> view.patch
```

