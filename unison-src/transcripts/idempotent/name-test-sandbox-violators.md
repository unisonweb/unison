``` ucm :hide
> builtins.mergeio
```

This passes, because the IO sandbox doesn't seem to apply to `test>` watch expressions.

``` unison
toException : Either Failure r ->{Exception} r
toException = cases
  Left e -> Exception.raise e
  Right a -> a

printLine : Text ->{IO, Exception} ()
printLine t =
  stdOut = stdHandle StdOut
  toException (putBytes.impl stdOut (toUtf8 t))
  toException (putBytes.impl stdOut (toUtf8 "\n"))

test> foo.test =
  x = 192
  coerceAbilities (do printLine "hello") ()
  [Ok "Passed"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo.test    : [Result]
  + printLine   : Text ->{IO, Exception} ()
  + toException : Either Failure r ->{Exception} r

  Run `update` to apply these changes to your codebase.

    13 |   x = 192
    
    ✅ Passed Passed
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

The `test` command succeeds, because the result of `foo.test` is already cached, which skips the IO sandbox.

``` ucm
> test

  Cached test results (`help testcache` to learn more)

    1. foo.test   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

This test which is essentially the same will fail, because it is never run with a `test>` watch expression to get the result into the test cache.

``` unison
bar.test : [Test.Result]
bar.test =
  x = 42
  unsafe.coerceAbilities (do printLine "hello") ()
  [Ok "Passed"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar.test : [Result]

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm :error
> test

    
    Cached test results (`help testcache` to learn more)
    
      1. foo.test   ◉ Passed
    
    ✅ 1 test(s) passing
    
    ✅  



  Error while evaluating test `bar.test`:

    ❗️
    
    Sorry – I’ve encountered a Unison runtime error.
    
      Attempted to use disallowed builtin in sandboxed environment: IO.stdHandle
    
    Please report it at
    https://github.com/unisonweb/unison/issues/new/choose.
```
