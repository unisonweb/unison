# tests for IO / MVar

Tests for IO builtins which wired to foreign haskell calls.

## Setup

You can skip the section which is just needed to make the transcript self-contained.

TempDirs/autoCleaned is an ability/hanlder which allows you to easily
create a scratch directory which will automatically get cleaned up.

```unison
use .builtin.io2 Failure

filter: (a -> Boolean) -> [a] -> [a]
filter f all =
  go acc = cases
    a +: as -> if (f a) then go (cons a acc) as else go acc as
    [] -> acc
  go [] all

ability Exception e where raise : e ->{Exception e} a

toException : Either e a ->{Exception e} a
toException = cases
    Left e  -> raise e
    Right a -> a


Exception.toEither.handler : Request {Exception e} a -> Either e a
Exception.toEither.handler = cases
    { a }          -> Right a
    {raise e -> _} -> Left e

Exception.toEither : '{g, Exception e} a ->{g} Either e a
Exception.toEither a = handle !a with Exception.toEither.handler

isNone = cases
  Some _ -> false
  None -> true

-- An ability that facilitates creating temoporary directories that can be 
-- automatically cleaned up
ability TempDirs where
  newTempDir: Text -> Either Failure Text
  removeDir: Text -> Either Failure ()

-- A handler for TempDirs which cleans up temporary directories
-- This will be useful for IO tests which need to interact with 
-- the filesystem
autoCleaned.handler: '{io2.IO} (Request {TempDirs} r -> r)
autoCleaned.handler _ =
  remover : [Text] -> {io2.IO} ()
  remover = cases
    a +: as -> removeDirectory a
               remover as
    [] -> ()

  go : [Text] -> {io2.IO} Request {TempDirs} r -> r
  go dirs = cases
   { a } -> remover dirs
            a
   { TempDirs.newTempDir prefix -> k } ->
      dir = createTempDirectory prefix
      match dir with
        Right dir' -> handle k dir with go (dir' +: dirs)
        Left _ -> handle k dir with go dirs

   { TempDirs.removeDir dir -> k } ->
     handle k (removeDirectory dir) with go (filter (d -> not (d == dir)) dirs)

  go []

autoCleaned: '{io2.IO, TempDirs} r -> r
autoCleaned comp = handle !comp with !autoCleaned.handler

ability Stream a where
   emit: a -> ()

Stream.toList.handler : Request {Stream a} r -> [a]
Stream.toList.handler =
  go : [a] -> Request {Stream a} r -> [a]
  go acc = cases
    { Stream.emit a -> k } -> handle !k with go (acc :+ a)
    { _ } -> acc

  go []

Stream.toList : '{Stream a} r -> [a]
Stream.toList s = handle !s with toList.handler

Stream.collect.handler : Request {Stream a} r -> ([a],r)
Stream.collect.handler =
  go : [a] -> Request {Stream a} r -> ([a],r)
  go acc = cases
    { Stream.emit a -> k } -> handle !k with go (acc :+ a)
    { r } -> (acc, r)

  go []

Stream.collect : '{e, Stream a} r -> {e} ([a],r)
Stream.collect s =
  handle !s with Stream.collect.handler

stdout = IO.stdHandle StdErr
printText : Text -> {io2.IO} Either Failure ()
printText t = putBytes stdout (toUtf8 t)

expect : Text -> (a -> a -> Boolean) -> a -> a -> {Stream Result} ()
expect msg compare expected actual = if compare expected actual then emit (Ok msg) else emit (Fail msg)

expectU : Text -> a -> a -> {Stream Result} ()
expectU msg expected actual = expect msg (==) expected actual

check: Text -> Boolean -> {Stream Result} ()
check msg test = if test then emit (Ok msg) else emit (Fail msg)

-- Run tests which might fail, might create temporary directores and Stream out
-- results, returns the Results and the result of the test
evalTest: '{Stream Result, Exception Failure, io2.IO, TempDirs} a -> ([Result], Either Failure a)
evalTest a = handle
               (handle
                 (handle !a with Exception.toEither.handler)
               with Stream.collect.handler)
             with !autoCleaned.handler

-- Run tests which might fail, might create temporary directores and Stream out
-- results, but ignore the produced value and only return the test Results
runTest: '{Stream Result, Exception Failure, io2.IO, TempDirs} a -> [Result]
runTest t = match evalTest t with
              (results, Right _) -> results
              (results, Left (Failure _ t)) -> results :+ (Fail t)

```

## Who watches the watchers?

First lets do some basic testing of our test harness to make sure its
working.

```unison
testAutoClean : '{io2.IO}[Result]
testAutoClean _ =
  go: '{Stream Result, Exception Failure, io2.IO, TempDirs} Text
  go _ =
    dir = toException (newTempDir "autoclean")
    check "our temporary directory should exist" (toException (isDirectory dir))
    dir

  match evalTest go with
    (results, Left (Failure _ t)) -> results :+ (Fail t)
    (results, Right dir) ->
       match isDirectory dir with
         Right b -> if b
                    then results :+ (Fail "our temporary directory should no longer exist")
                    else results :+ (Ok "our temporary directory should no longer exist")
         Left (Failure _ t) -> results :+ (Fail t)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testAutoClean : '{io2.IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testAutoClean : '{io2.IO} [Result]

.> io.test testAutoClean

    New test results:
  
  ◉ testAutoClean   our temporary directory should exist
  ◉ testAutoClean   our temporary directory should no longer exist
  
  ✅ 2 test(s) passing
  
  Tip: Use view testAutoClean to view the source of a test.

```
## Basic File Functions

### Creating/Deleting/Renaming Directories

Tests: createDirectory, 
       isDirectory, 
       fileExists, 
       renameDirectory,
       deleteDirectory

```unison
testCreateRename : '{io2.IO} [Result]
testCreateRename _ =
  test = 'let
    tempDir = toException (newTempDir "fileio")
    fooDir = tempDir ++ "/foo"
    barDir = tempDir ++ "/bar"
    toException let createDirectory fooDir
    check "create a foo directory" (toException (isDirectory fooDir))
    check "directory should exist" (toException (fileExists fooDir))
    toException let renameDirectory fooDir barDir
    check "foo should no longer exist" (not (toException (fileExists fooDir)))
    check "directory should no longer exist" (not (toException (fileExists fooDir)))
    check "bar should now exist" (toException (fileExists barDir))

    bazDir = barDir ++ "/baz"
    toException let createDirectory bazDir
    toException let removeDirectory barDir

    check "removeDirectory works recursively" (not (toException (isDirectory barDir)))
    check "removeDirectory works recursively" (not (toException (isDirectory bazDir)))

  runTest test
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testCreateRename : '{io2.IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testCreateRename : '{io2.IO} [Result]

.> io.test testCreateRename

    New test results:
  
  ◉ testCreateRename   create a foo directory
  ◉ testCreateRename   directory should exist
  ◉ testCreateRename   foo should no longer exist
  ◉ testCreateRename   directory should no longer exist
  ◉ testCreateRename   bar should now exist
  ◉ testCreateRename   removeDirectory works recursively
  ◉ testCreateRename   removeDirectory works recursively
  
  ✅ 7 test(s) passing
  
  Tip: Use view testCreateRename to view the source of a test.

```
### Opening / Closing files

Tests: openFile
       closeFile
       isFileOpen

```unison
testOpenClose : '{io2.IO} [Result]
testOpenClose _ =
  test = 'let
    tempDir = toException (newTempDir "seek")
    fooFile = tempDir ++ "/foo"
    handle1 = toException (openFile fooFile FileMode.Write)
    check "file should be open" (toException (isFileOpen handle1))
    toException (closeFile handle1)
    check "file should be closed" (not (toException (isFileOpen handle1)))

  runTest test
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testOpenClose : '{io2.IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testOpenClose : '{io2.IO} [Result]

.> io.test testOpenClose

    New test results:
  
  ◉ testOpenClose   file should be open
  ◉ testOpenClose   file should be closed
  
  ✅ 2 test(s) passing
  
  Tip: Use view testOpenClose to view the source of a test.

```
### Seeking in open files

Tests: openFile
       putBytes
       closeFile
       isSeekable
       isFileEOF
       seekHandle
       getBytes

```unison
testSeek : '{io2.IO} [Result]
testSeek _ =
  test = 'let
    tempDir = toException (newTempDir "seek")
    fooFile = tempDir ++ "/foo"
    handle1 = toException (openFile fooFile FileMode.Write)
    putBytes handle1 (toUtf8 "12345678")
    closeFile handle1

    handle3 = toException (openFile fooFile FileMode.Read)
    check "readable file should be seekable" (toException (isSeekable handle3))
    check "shouldn't be the EOF" (not (toException (isFileEOF handle3)))
    expectU "we should be at position 0" +0 (toException (handlePosition handle3))

    toException (seekHandle handle3 AbsoluteSeek +1)
    expectU "we should be at position 1" +1 (toException (handlePosition handle3))
    bytes3a = toException (getBytes handle3 1000)
    text3a = toException (Text.fromUtf8 bytes3a)
    expectU "should be able to read our temporary file after seeking" "2345678" text3a
    closeFile handle3

  runTest test

testAppend : '{io2.IO} [Result]
testAppend _ =
  test = 'let
    tempDir = toException (newTempDir "openFile")

    fooFile = tempDir ++ "/foo"
    handle1 = toException (openFile fooFile FileMode.Write)
    putBytes handle1 (toUtf8 "test1")
    closeFile handle1

    handle2 = toException (openFile fooFile FileMode.Append)
    putBytes handle2 (toUtf8 "test2")
    expectU "we should be at position 4" +4 (toException (handlePosition handle2))
    check "which is the EOF" (toException (isFileEOF handle2))
    closeFile handle2

    handle3 = toException (openFile fooFile FileMode.Read)
    bytes3 = toException (getBytes handle3 1000)
    text3 = toException (Text.fromUtf8 bytes3)
    expectU "should be able to read our temporary file" "test1test2" text3

    closeFile handle3

  runTest test
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testAppend : '{io2.IO} [Result]
      testSeek   : '{io2.IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testAppend : '{io2.IO} [Result]
    testSeek   : '{io2.IO} [Result]

.> io.test testSeek

    New test results:
  
  ◉ testSeek   readable file should be seekable
  ◉ testSeek   shouldn't be the EOF
  
  ✅ 2 test(s) passing
  
  Tip: Use view testSeek to view the source of a test.

.> io.test testAppend

    New test results:
  
  😶 No tests available.

```
### SystemTime
```unison
testSystemTime : '{io2.IO} [Result]
testSystemTime _ =
  test = 'let
    t = toException !io2.IO.systemTime
    check "systemTime should be sane" ((t > 1600000000) && (t < 2000000000))

  runTest test
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testSystemTime : '{io2.IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testSystemTime : '{io2.IO} [Result]

.> io.test testSystemTime

    New test results:
  
  ◉ testSystemTime   systemTime should be sane
  
  ✅ 1 test(s) passing
  
  Tip: Use view testSystemTime to view the source of a test.

```
## MVars

MVars are threadsafe mutable locations which at any time may or may not
contain a signle typed value. They are a building block on which many
concurrency primitives can be built that allow multiple threads to 
synchronize and share data.

