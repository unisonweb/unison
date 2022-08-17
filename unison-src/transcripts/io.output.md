# tests for built-in IO functions

Tests for IO builtins which wired to foreign haskell calls.

## Setup

You can skip the section which is just needed to make the transcript self-contained.

TempDirs/autoCleaned is an ability/hanlder which allows you to easily
create a scratch directory which will automatically get cleaned up.

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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testCreateRename : '{IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testCreateRename : '{IO} [Result]

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
    tempDir = (newTempDir "seek")
    fooFile = tempDir ++ "/foo"
    handle1 = openFile fooFile FileMode.Write
    check "file should be open" (isFileOpen handle1)
    setBuffering handle1 (SizedBlockBuffering 1024)
    check "file handle buffering should match what we just set." (getBuffering handle1 == SizedBlockBuffering 1024)
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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testOpenClose : '{IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testOpenClose : '{IO} [Result]

.> io.test testOpenClose

    New test results:
  
  ◉ testOpenClose   file should be open
  ◉ testOpenClose   file handle buffering should match what we just set.
  ◉ testOpenClose   file should be closed
  ◉ testOpenClose   file should be closed
  
  ✗ testOpenClose   bytes have been written
  ✗ testOpenClose   bytes have been written
  
  🚫 2 test(s) failing, ✅ 4 test(s) passing
  
  Tip: Use view testOpenClose to view the source of a test.

```



🛑

The transcript failed due to an error in the stanza above. The error is:


    New test results:
  
  ◉ testOpenClose   file should be open
  ◉ testOpenClose   file handle buffering should match what we just set.
  ◉ testOpenClose   file should be closed
  ◉ testOpenClose   file should be closed
  
  ✗ testOpenClose   bytes have been written
  ✗ testOpenClose   bytes have been written
  
  🚫 2 test(s) failing, ✅ 4 test(s) passing
  
  Tip: Use view testOpenClose to view the source of a test.

