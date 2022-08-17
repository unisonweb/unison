# tests for io2.MVar

`MVar`s are mutable, sharable storage for a single value, which may or
may not be present at any given time. It is sharable in the sense that
it is safe for multiple threads to attempt simultaneous reading and
writing to and from the same MVar safely.

MVars are the building block on which many other concurrency
primitives can be built, such as Futures, Run at most once initializer
blocks, Queues, etc.


```unison
eitherCk : (a -> Boolean) -> Either e a -> Boolean
eitherCk f = cases
  Left _ -> false
  Right x -> f x

testMvars: '{io2.IO}[Result]
testMvars _ =
  test = 'let
    test = "test"
    test2 = "test2"
    ma = MVar.new test
    check "ma should not be empty" (not (isEmpty ma))
    test0 = read ma
    test1 = take ma
    expectU "should read what you sow" test test0
    expectU "should reap what you sow" test test1
    check "ma should be empty" (isEmpty ma)
    put ma test
    test'' = swap ma test2
    expectU "swap returns old contents" test test''
    test''' = swap ma test
    expectU "swap returns old contents" test2 test'''

    ma2 = !MVar.newEmpty
    check "tryRead should succeed when not empty"
      (eitherCk (x -> not (isNone x)) (tryRead.impl ma))
    check "tryPut should fail when not empty"
      (eitherCk (b -> not b) (tryPut.impl ma test))
    check "tryTake should succeed when not empty" (not (isNone (tryTake ma)))
    check "tryTake should not succeed when empty" (isNone (tryTake ma))

    check "ma2 should be empty" (isEmpty ma2)
    check "tryTake should fail when empty" (isNone (tryTake ma2))
    check "tryRead should fail when empty"
      (eitherCk isNone (tryRead.impl ma2))


  runTest test
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      eitherCk  : (a ->{g} Boolean) -> Either e a ->{g} Boolean
      testMvars : '{IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    eitherCk  : (a ->{g} Boolean) -> Either e a ->{g} Boolean
    testMvars : '{IO} [Result]

.> io.test testMvars

    New test results:
  
  ◉ testMvars   ma should not be empty
  ◉ testMvars   ma should be empty
  ◉ testMvars   tryRead should succeed when not empty
  ◉ testMvars   tryPut should fail when not empty
  ◉ testMvars   tryTake should succeed when not empty
  ◉ testMvars   tryTake should not succeed when empty
  ◉ testMvars   ma2 should be empty
  ◉ testMvars   tryTake should fail when empty
  ◉ testMvars   tryRead should fail when empty
  
  ✗ testMvars   should read what you sow
  ✗ testMvars   should reap what you sow
  ✗ testMvars   swap returns old contents
  ✗ testMvars   swap returns old contents
  
  🚫 4 test(s) failing, ✅ 9 test(s) passing
  
  Tip: Use view testMvars to view the source of a test.

```



🛑

The transcript failed due to an error in the stanza above. The error is:


    New test results:
  
  ◉ testMvars   ma should not be empty
  ◉ testMvars   ma should be empty
  ◉ testMvars   tryRead should succeed when not empty
  ◉ testMvars   tryPut should fail when not empty
  ◉ testMvars   tryTake should succeed when not empty
  ◉ testMvars   tryTake should not succeed when empty
  ◉ testMvars   ma2 should be empty
  ◉ testMvars   tryTake should fail when empty
  ◉ testMvars   tryRead should fail when empty
  
  ✗ testMvars   should read what you sow
  ✗ testMvars   should reap what you sow
  ✗ testMvars   swap returns old contents
  ✗ testMvars   swap returns old contents
  
  🚫 4 test(s) failing, ✅ 9 test(s) passing
  
  Tip: Use view testMvars to view the source of a test.

