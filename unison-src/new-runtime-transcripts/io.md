# tests for IO / MVar

```ucm:hide
.> builtins.merge
.> builtins.mergeio
.> cd builtin
```
Tests for IO builtins which wired to foreign haskell calls.

## Setup

You can skip the section which is just needed to make the transcript self-contained.

TempDirs/autoCleaned is an ability/hanlder which allows you to easily
create a scratch directory which will automatically get cleaned up.

```unison:hide
use .builtin.io2 Failure

ability TempDirs where
  newTempDir: Text -> Either Failure Text
  removeDir: Text -> Either Failure ()

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

toEither : '{g, Exception e} a ->{g} Either e a
toEither a =
  handler : Request {Exception e} a -> Either e a
  handler = cases
    { a }          -> Right a
    {raise e -> _} -> Left e

  handle !a with handler

autoCleaned: '{io2.IO, TempDirs} r -> r
autoCleaned comp =
  remover : [Text] -> {io2.IO} ()
  remover = cases
    a +: as -> removeDirectory a
               remover as
    [] -> ()

  handler : [Text] -> {io2.IO} Request {TempDirs} r -> r
  handler dirs = cases
   { a } -> remover dirs
            a
   { TempDirs.newTempDir prefix -> k } ->
      dir = createTempDirectory prefix
      match dir with
        Right dir' -> handle k dir with handler (dir' +: dirs)
        Left _ -> handle k dir with handler dirs

   { TempDirs.removeDir dir -> k } ->
      handle k (removeDirectory dir) with handler (filter (d -> not (d == dir)) dirs)

  handle !comp with handler []

stdout = IO.stdHandle StdErr
printText : Text -> {io2.IO} Either Failure ()
printText t = putBytes stdout (toUtf8 t)

iHope : Text -> Boolean -> {io2.IO}()
iHope that isIt = if isIt then () else
  printText ("FAILURE: " ++ that ++ "\n")
  ()
```

```ucm:hide
.> add
```

## Who watches the watchers?

First lets test our support code (which in turn will test some of our
IO functions)

```unison
testAutoClean : '{io2.IO}()
testAutoClean _ =
  go: '{io2.IO, TempDirs} (Either Failure Text)
  go _ = toEither 'let
    -- if this doesn't work, nothing will, because newTempDir will fail
--    tempDir = toException (!getTempDirectory)
    dir = toException (newTempDir "autoclean")

    iHope "our temporary directory should exist" (toException (isDirectory dir))
    dir

  printText "testing autoClean..."
--  match autoCleaned go with
--    Left (Failure _ t) -> printText t
--                          ()
--    Right dir -> match IO.fileExists dir with
--        Right b -> iHope "our temporary directory should no longer exist" (not b)
--        Left (Failure _ t) -> printText t
--                              ()
--  printText "done testing autoClean."
  ()
```

```ucm
.> add
.> run testAutoClean
```

## Basic File Functions
