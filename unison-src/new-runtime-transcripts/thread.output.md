## Setup

You can skip the section which is just needed to make the transcript self-contained.

```unison
use .builtin.io2 Failure
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

-- Run tests which might fail, might create temporary directores and Stream out
-- results, returns the Results and the result of the test
evalTest: '{Stream Result, Exception Failure, io2.IO} a -> ([Result], Either Failure a)
evalTest a = handle
               (handle !a with Exception.toEither.handler)
             with Stream.collect.handler

-- Run tests which might fail, might create temporary directores and Stream out
-- results, but ignore the produced value and only return the test Results
runTest: '{Stream Result, Exception Failure, io2.IO} a -> [Result]
runTest t = match evalTest t with
              (results, Right _) -> results
              (results, Left (Failure _ t _)) -> results :+ (Fail t)


--
-- convenience functions for emitting test results
--
expect : (a -> Text) -> (a -> a -> Boolean) -> Text -> a -> a -> {Stream Result} ()
expect toText compare msg expected actual = let
  if (compare expected actual) then 
    emit (Ok msg) 
  else let
    failMsg = msg ++ "expected : " ++ (toText expected) ++ " actual: " ++ (toText actual)
    emit (Fail failMsg)

expectU : (a -> Text) -> Text -> a -> a -> {Stream Result} ()
expectU toText msg expected actual = expect toText (==) msg expected actual

check: Text -> Boolean -> {Stream Result} ()
check msg test = if test then emit (Ok msg) else emit (Fail msg)
```

Lets just make sure we can start a thread

```unison
otherThread : '{io2.IO}()
otherThread = 'let
  watch "I'm the other Thread" ()

testBasicFork : '{io2.IO} [Result]
testBasicFork = 'let
  test = 'let
    watch "I'm the parent thread" ()
    threadId = .builtin.io2.IO.forkComp otherThread
    emit (Ok "created thread")

  runTest test

```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      otherThread   : '{io2.IO} ()
      testBasicFork : '{io2.IO} [Result]

```
See if we can get another thread to stuff a value into a MVar

```unison
thread1 : MVar Nat -> '{io2.IO}()
thread1 mv = 'let
  go : '{io2.IO, Exception Failure} ()
  go = 'let
    x = toException (take.impl mv)
    toException (put.impl mv (increment x))

  match (toEither go) with 
    Left (Failure _ t _) -> watch t ()
    _ -> ()


testBasicMultiThreadMVar : '{io2.IO} [Result]
testBasicMultiThreadMVar = 'let
  test: '{io2.IO, Exception Failure, Stream Result} ()
  test = 'let
    mv = new 10
    .builtin.io2.IO.forkComp (thread1 mv)
    next = toException (take.impl mv)
    expectU Nat.toText "other thread should have incremented" 11 next

  runTest test


```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testBasicMultiThreadMVar : '{io2.IO} [Result]
      thread1                  : MVar Nat -> '{io2.IO} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testBasicMultiThreadMVar : '{io2.IO} [Result]
    thread1                  : MVar Nat -> '{io2.IO} ()

.> io.test testBasicMultiThreadMVar

    New test results:
  
  ◉ testBasicMultiThreadMVar   other thread should have incremented
  
  ✅ 1 test(s) passing
  
  Tip: Use view testBasicMultiThreadMVar to view the source of a
       test.

```
```unison
sendingThread: Nat -> MVar Nat -> '{io2.IO}()
sendingThread toSend mv = 'let
  go : '{io2.IO, Exception Failure} ()
  go = 'let
    toException (put.impl mv (increment toSend))
    
  match (toEither go) with
    Left (Failure _ t _) -> watch t ()
    _ -> ()


receivingThread: MVar Nat -> MVar Text -> '{io2.IO}()
receivingThread recv send = 'let
  go : '{io2.IO, Exception Failure} ()
  go = 'let
    recvd = toException (take.impl recv)
    toException (put.impl send (toText recvd))
    
  match (toEither go) with
    Left (Failure _ t _) -> watch t ()
    _ -> ()
  
testTwoThreads: '{io2.IO}[Result]
testTwoThreads = 'let
  test = 'let
    send = !MVar.newEmpty
    recv = !MVar.newEmpty

    .builtin.io2.IO.forkComp (sendingThread 6 send)
    .builtin.io2.IO.forkComp (receivingThread send recv)

    recvd = toException (take.impl recv)

    expectU (x->x) "" "7" recvd

  runTest test

```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      receivingThread : MVar Nat -> MVar Text -> '{io2.IO} ()
      sendingThread   : Nat -> MVar Nat -> '{io2.IO} ()
      testTwoThreads  : '{io2.IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    receivingThread : MVar Nat -> MVar Text -> '{io2.IO} ()
    sendingThread   : Nat -> MVar Nat -> '{io2.IO} ()
    testTwoThreads  : '{io2.IO} [Result]

.> io.test testTwoThreads

    New test results:
  
  ◉ testTwoThreads   
  
  ✅ 1 test(s) passing
  
  Tip: Use view testTwoThreads to view the source of a test.

```
