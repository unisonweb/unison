# tests for io2.MVar

```ucm:hide
.> builtins.merge
.> builtins.mergeio
.> cd builtin

## Setup

You can skip the section which is just needed to make the transcript self-contained.

```unison:hide
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

```ucm:hide
.> add
```

`MVar`s are mutable, sharable storage for a single value, which may or
may not be present at any given time. It is sharable in the sense that
it is safe for multiple threads to attempt simultaneous reading and
writing to and from the same MVar safely.

MVars are the building block on which many other concurrency
primitives can be built, such as Futures, Run at most once initializer
blocks, Queues, etc.


```unison
testMvars: '{io2.IO}[Result]
testMvars _ =
  test = 'let
    test = "test"
    test2 = "test2"
    ma = MVar.new test
    check "ma should not be empty" (not (isEmpty ma))
    test' = toException (take ma)
    expectU "should reap what you sow" test test'
    check "ma should be empty" (isEmpty ma)
    toException (put ma test)
    test'' = toException (swap ma test2)
    expectU "swap returns old contents" test test''
    test''' = toException (swap ma test)
    expectU "swap returns old contents" test2 test'''

    ma2 = !MVar.newEmpty
    check "tryTake should succeed when not empty" (not (isNone (tryTake ma)))
    check "tryTake should not succeed when empty" (isNone (tryTake ma))

    check "ma2 should be empty" (isEmpty ma2)
    check "tryTake should fail when empty" (isNone (tryTake ma2))


  runTest test
```
```ucm
.> add
.> io.test testMvars
```

