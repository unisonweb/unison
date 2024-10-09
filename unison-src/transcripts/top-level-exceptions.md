A simple transcript to test the use of exceptions that bubble to the top level.

``` ucm :hide
scratch/main> builtins.merge
```

FYI, here are the `Exception` and `Failure` types:

``` ucm
scratch/main> view Exception Failure
```

Here's a sample program just to verify that the typechecker allows `run` to throw exceptions:

``` unison
use builtin IO Exception Test.Result

main : '{IO, Exception} ()
main _ = ()

mytest : '{IO, Exception} [Test.Result]
mytest _ = [Ok "Great"]
```

``` ucm
scratch/main> run main
scratch/main> add
scratch/main> io.test mytest
```

Now a test to show the handling of uncaught exceptions:

``` unison
main2 = '(error "oh noes!" ())

error : Text -> a ->{Exception} x
error msg a =
  builtin.Exception.raise (Failure (typeLink RuntimeError) msg (Any a))

unique type RuntimeError =
```

``` ucm :error
scratch/main> run main2
```
