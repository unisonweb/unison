A simple transcript to test the use of exceptions that bubble to the top level.

``` ucm :hide
scratch/main> builtins.merge
```

FYI, here are the `Exception` and `Failure` types:

``` ucm
scratch/main> view Exception Failure

  structural ability builtin.Exception where
    raise : Failure ->{Exception} x

  type builtin.io2.Failure = Failure Type Text Any
```

Here's a sample program just to verify that the typechecker allows `run` to throw exceptions:

``` unison
use builtin IO Exception Test.Result

main : '{IO, Exception} ()
main _ = ()

mytest : '{IO, Exception} [Test.Result]
mytest _ = [Ok "Great"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + main   : '{IO, Exception} ()
  + mytest : '{IO, Exception} [Result]

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run main

  ()

scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> io.test mytest

    New test results:

    1. mytest   ◉ Great

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

Now a test to show the handling of uncaught exceptions:

``` unison
main2 = '(error "oh noes!" ())

error : Text -> a ->{Exception} x
error msg a =
  builtin.Exception.raise (Failure (typeLink RuntimeError) msg (Any a))

unique type RuntimeError =
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type RuntimeError

  + error : Text -> a ->{Exception} x
  + main2 : '{Exception} r

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
scratch/main> run main2

  💔💥

  The program halted with an unhandled exception:

    Failure (typeLink RuntimeError) "oh noes!" (Any ())

  Stack trace:
    ##raise
```
