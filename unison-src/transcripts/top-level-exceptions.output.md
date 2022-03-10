
A simple transcript to test the use of exceptions that bubble to the top level.

FYI, here are the `Exception` and `Failure` types:

```ucm
.> view Exception Failure

  structural ability builtin.Exception where
    raise : Failure ->{builtin.Exception} x
  
  unique type builtin.io2.Failure
    = Failure Type Text Any

```
Here's a sample program just to verify that the typechecker allows `run` to throw exceptions:

```unison
use builtin IO Exception Test.Result

main : '{IO, Exception} ()
main _ = ()

mytest : '{IO, Exception} [Test.Result]
mytest _ = [Ok "Great"]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      main   : '{IO, Exception} ()
      mytest : '{IO, Exception} [Result]

```
```ucm
.> run main

.> add

  ⍟ I've added these definitions:
  
    main   : '{IO, Exception} ()
    mytest : '{IO, Exception} [Result]

.> io.test mytest

    New test results:
  
  ◉ mytest   Great
  
  ✅ 1 test(s) passing
  
  Tip: Use view mytest to view the source of a test.

```
Now a test to show the handling of uncaught exceptions:

```unison
main2 = '(error "oh noes!" ())

error : Text -> a ->{Exception} x
error msg a =
  builtin.Exception.raise (Failure (typeLink RuntimeError) msg (Any a))

unique type RuntimeError =
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type RuntimeError
      error : Text -> a ->{Exception} x
      main2 : '{Exception} r

```
```ucm
.> run main2

  💔💥
  
  The program halted with an unhandled exception:
  
    Failure (typeLink RuntimeError) "oh noes!" (Any ())

```
