The `io.test` command should run all of the tests within the current namespace, excluding libs.

```unison
-- We manually specify types so we don't need to pull in base to run IO and such
ioAndExceptionTest : '{IO, Exception} [Result]
ioAndExceptionTest = do 
  [Ok "Success"]

exceptionTest : '{Exception} [Result]
exceptionTest = do 
  [Ok "Success"]

ioTest : '{IO} [Result]
ioTest = do 
  [Ok "Success"]

lib.ioAndExceptionTestInLib  : '{IO, Exception} [Result]
lib.ioAndExceptionTestInLib  = do 
  [Ok "Success"]
```

Run a IO tests one by one 

```ucm
.> io.test ioAndExceptionTest

    New test results:
  
  ◉ ioAndExceptionTest   Success
  
  ✅ 1 test(s) passing
  
  Tip: Use view ioAndExceptionTest to view the source of a test.

.> io.test exceptionTest

    New test results:
  
  ◉ exceptionTest   Success
  
  ✅ 1 test(s) passing
  
  Tip: Use view exceptionTest to view the source of a test.

.> io.test ioTest

    New test results:
  
  ◉ ioTest   Success
  
  ✅ 1 test(s) passing
  
  Tip: Use view ioTest to view the source of a test.

```
`io.test` doesn't cache results

```ucm
.> io.test ioAndExceptionTest

    New test results:
  
  ◉ ioAndExceptionTest   Success
  
  ✅ 1 test(s) passing
  
  Tip: Use view ioAndExceptionTest to view the source of a test.

```
`io.test.all` will run all matching tests except those in the `lib` namespace.

```ucm
.> io.test.all

  

  

  

  

  

  

    New test results:
  
  ◉ exceptionTest        Success
  ◉ ioAndExceptionTest   Success
  ◉ ioTest               Success
  
  ✅ 3 test(s) passing
  
  Tip: Use view exceptionTest to view the source of a test.

```
