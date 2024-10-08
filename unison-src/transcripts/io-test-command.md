``` ucm :hide
scratch/main> builtins.merge
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

lib.ioAndExceptionTestInLib  : '{IO, Exception} [Result]
lib.ioAndExceptionTestInLib  = do
  [Ok "Success"]
```

``` ucm :hide
scratch/main> add
```

Run a IO tests one by one

``` ucm
scratch/main> io.test ioAndExceptionTest
scratch/main> io.test ioTest
```

`io.test` doesn't cache results

``` ucm
scratch/main> io.test ioAndExceptionTest
```

`io.test.all` will run all matching tests except those in the `lib` namespace.

``` ucm
scratch/main> io.test.all
```
