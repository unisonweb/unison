``` ucm :hide
> builtins.merge
```

The `io.test` command should run all of the tests within the current namespace, excluding libs.

``` unison :hide
-- We manually specify types so we don't need to pull in base to run IO and such
ioAndExceptionTest : '{IO, Exception} [Result]
ioAndExceptionTest = do
  [Ok "Success"]

ioTest : '{IO} [Result]
ioTest = do
  [Ok "Success"]

ioAndExceptionTestInLib  : '{IO, Exception} [Result]
ioAndExceptionTestInLib  = do
  [Ok "Success"]
```

``` ucm :hide
> update

> move ioAndExceptionTestInLib lib.ioAndExceptionTestInLib
```

Run a IO tests one by one

``` ucm
> io.test ioAndExceptionTest

    New test results:

    1. ioAndExceptionTest   ◉ Success

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.

> io.test ioTest

    New test results:

    1. ioTest   ◉ Success

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`io.test` doesn't cache results

``` ucm
> io.test ioAndExceptionTest

    New test results:

    1. ioAndExceptionTest   ◉ Success

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`io.test.all` will run all matching tests except those in the `lib` namespace.

``` ucm
> io.test.all





    New test results:

    1. ioAndExceptionTest   ◉ Success
    2. ioTest               ◉ Success

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`io.test` can run a test of the scratch file that hasn't been added yet. As for `run`, the file's names shadow the codebase's and its code is looked up first.

``` unison :hide
scratchHelper : Nat -> Nat
scratchHelper n = n + 1

scratchTest : '{IO, Exception} [Result]
scratchTest = do
  if scratchHelper 1 == 2 then [Ok "Success"] else [Fail "Failure"]

ioTest : '{IO} [Result]
ioTest = do
  [Ok "Success from the scratch file"]
```

``` ucm
> io.test scratchTest

    New test results:

    1. scratchTest   ◉ Success

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.

> io.test ioTest

    New test results:

    1. ioTest   ◉ Success from the scratch file

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
