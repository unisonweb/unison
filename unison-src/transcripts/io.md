# tests for built-in IO functions

```ucm:hide
.> builtins.merge
.> builtins.mergeio
.> load unison-src/transcripts-using-base/base.u
.> add
.> cd builtin
```

Tests for IO builtins which wired to foreign haskell calls.

## Setup

You can skip the section which is just needed to make the transcript self-contained.

TempDirs/autoCleaned is an ability/hanlder which allows you to easily
create a scratch directory which will automatically get cleaned up.

```ucm:hide
.> add
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
    tempDir = newTempDir "fileio"
    fooDir = tempDir ++ "/foo"
    barDir = tempDir ++ "/bar"
    createDirectory.impl fooDir
    check "create a foo directory" (isDirectory fooDir)
    check "directory should exist" (fileExists fooDir)
    renameDirectory fooDir barDir
    check "foo should no longer exist" (not (fileExists fooDir))
    check "directory should no longer exist" (not (fileExists fooDir))
    check "bar should now exist" (fileExists barDir)

    bazDir = barDir ++ "/baz"
    createDirectory.impl bazDir
    removeDirectory.impl barDir

    check "removeDirectory works recursively" (not (isDirectory barDir))
    check "removeDirectory works recursively" (not (isDirectory bazDir))

  runTest test
```

```ucm
.> add
.> io.test testCreateRename
```

### Opening / Closing files

Tests: openFile
       closeFile
       isFileOpen

```unison
testOpenClose : '{io2.IO} [Result]
testOpenClose _ =
  test = 'let
    tempDir = (newTempDir "seek")
    fooFile = tempDir ++ "/foo"
    handle1 = openFile fooFile FileMode.Write
    check "file should be open" (isFileOpen handle1)
    setBuffering handle1 (SizedBlockBuffering 1024)
    setBuffering handle1 (getBuffering handle1)
    putBytes handle1 0xs01
    setBuffering handle1 NoBuffering
    setBuffering handle1 (getBuffering handle1)
    putBytes handle1 0xs23
    setBuffering handle1 BlockBuffering
    setBuffering handle1 (getBuffering handle1)
    putBytes handle1 0xs45
    setBuffering handle1 LineBuffering
    setBuffering handle1 (getBuffering handle1)
    putBytes handle1 0xs67
    closeFile handle1
    check "file should be closed" (not (isFileOpen handle1))

    -- make sure the bytes have been written
    handle2 = openFile fooFile FileMode.Read
    check "bytes have been written" (getBytes handle2 4 == 0xs01234567)
    closeFile handle2

    -- checking that ReadWrite mode works fine
    handle3 = openFile fooFile FileMode.ReadWrite
    check "bytes have been written" (getBytes handle3 4 == 0xs01234567)
    closeFile handle3

    check "file should be closed" (not (isFileOpen handle1))

  runTest test
```

```ucm
.> add
```
-- getBuffering.impl is currently broken on trunk (https://github.com/unisonweb/unison/issues/2767)
-- Add this back to the ucm block once that's fixed.
.> io.test testOpenClose

### Seeking in open files

Tests: openFile
       putBytes
       closeFile
       isSeekable
       isFileEOF
       seekHandle
       getBytes
       getLine

```unison
testSeek : '{io2.IO} [Result]
testSeek _ =
  test = 'let
    tempDir = newTempDir "seek"
    emit (Ok "seeked")
    fooFile = tempDir ++ "/foo"
    handle1 = openFile fooFile FileMode.Append
    putBytes handle1 (toUtf8 "12345678")
    closeFile handle1

    handle3 = openFile fooFile FileMode.Read
    check "readable file should be seekable" (isSeekable handle3)
    check "shouldn't be the EOF" (not (isFileEOF handle3))
    expectU "we should be at position 0" 0 (handlePosition handle3)

    seekHandle handle3 AbsoluteSeek +1
    expectU "we should be at position 1" 1 (handlePosition handle3)
    bytes3a = getBytes handle3 1000
    text3a = Text.fromUtf8 bytes3a
    expectU "should be able to read our temporary file after seeking" "2345678" text3a
    closeFile handle3

    barFile = tempDir ++ "/bar"
    handle4 = openFile barFile FileMode.Append
    putBytes handle4 (toUtf8 "foobar\n")
    closeFile handle4

    handle5 = openFile barFile FileMode.Read
    expectU "getLine should get a line" "foobar" (getLine handle5)
    closeFile handle5

  runTest test

testAppend : '{io2.IO} [Result]
testAppend _ =
  test = 'let
    tempDir = newTempDir "openFile"
    fooFile = tempDir ++ "/foo"
    handle1 = openFile fooFile FileMode.Write
    putBytes handle1 (toUtf8 "test1")
    closeFile handle1

    handle2 = openFile fooFile FileMode.Append
    putBytes handle2 (toUtf8 "test2")
    closeFile handle2

    handle3 = openFile fooFile FileMode.Read
    bytes3 = getBytes handle3 1000
    text3 = Text.fromUtf8 bytes3

    expectU "should be able to read our temporary file" "test1test2" text3

    closeFile handle3

  runTest test
```

```ucm
.> add
.> io.test testSeek
.> io.test testAppend
```

### SystemTime
```unison
testSystemTime : '{io2.IO} [Result]
testSystemTime _ =
  test = 'let
    t = !systemTime
    check "systemTime should be sane" ((t > 1600000000) && (t < 2000000000))

  runTest test
```

```ucm
.> add
.> io.test testSystemTime
```

### Get temp directory

```unison:hide
testGetTempDirectory : '{io2.IO} [Result]
testGetTempDirectory _ =
  test = 'let
    tempDir = reraise !getTempDirectory.impl
    check "Temp directory is directory" (isDirectory tempDir)
    check "Temp directory should exist" (fileExists tempDir)
  runTest test
```

```ucm
.> add
.> io.test testGetTempDirectory
```

### Get current directory

```unison:hide
testGetCurrentDirectory : '{io2.IO} [Result]
testGetCurrentDirectory _ =
  test = 'let
    currentDir = reraise !getCurrentDirectory.impl
    check "Current directory is directory" (isDirectory currentDir)
    check "Current directory should exist" (fileExists currentDir)
  runTest test
```

```ucm
.> add
.> io.test testGetCurrentDirectory
```

### Get directory contents

```unison:hide
testDirContents : '{io2.IO} [Result]
testDirContents _ =
  test = 'let
    tempDir = newTempDir "dircontents"
    c = reraise (directoryContents.impl tempDir)
    check "directory size should be"  (size c == 2)
    check "directory contents should have current directory and parent" let
      (c == [".", ".."]) || (c == ["..", "."])
  runTest test
```

```ucm
.> add
.> io.test testDirContents
```

### Read environment variables

```unison:hide
testHomeEnvVar : '{io2.IO} [Result]
testHomeEnvVar _ =
  test = 'let
    home = reraise (getEnv.impl "HOME")
    check "HOME environent variable should be set"  (size home > 0)
    match getEnv.impl "DOESNTEXIST" with 
      Right _ -> emit (Fail "env var shouldn't exist")
      Left _ -> emit (Ok "DOESNTEXIST didn't exist")
  runTest test
```
```ucm
.> add
.> io.test testHomeEnvVar
```

### Read command line args

`runMeWithNoArgs`, `runMeWithOneArg`, and `runMeWithTwoArgs` raise exceptions 
unless they called with the right number of arguments.

```unison:hide
testGetArgs.fail : Text -> Failure
testGetArgs.fail descr = Failure (typeLink IOFailure) descr !Any

testGetArgs.runMeWithNoArgs : '{io2.IO, Exception} ()
testGetArgs.runMeWithNoArgs = 'let
  args = reraise !getArgs.impl
  match args with
    [] -> printLine "called with no args"
    _ -> raise (fail "called with args")

testGetArgs.runMeWithOneArg : '{io2.IO, Exception} ()
testGetArgs.runMeWithOneArg = 'let
  args = reraise !getArgs.impl
  match args with
    [] -> raise (fail "called with no args")
    [_] -> printLine "called with one arg"
    _ -> raise (fail "called with too many args")

testGetArgs.runMeWithTwoArgs : '{io2.IO, Exception} ()
testGetArgs.runMeWithTwoArgs = 'let
  args = reraise !getArgs.impl
  match args with
    [] -> raise (fail "called with no args")
    [_] -> raise (fail "called with one arg")
    [_, _] -> printLine "called with two args"
    _ -> raise (fail "called with too many args")
```

Test that they can be run with the right number of args.
```ucm
.> add
.> cd testGetArgs
.> run runMeWithNoArgs
.> run runMeWithOneArg foo
.> run runMeWithTwoArgs foo bar
```

Calling our examples with the wrong number of args will error.

```ucm:error
.> run runMeWithNoArgs foo
```

```ucm:error
.> run runMeWithOneArg
```
```ucm:error
.> run runMeWithOneArg foo bar
```

```ucm:error
.> run runMeWithTwoArgs
```
