Merge builtins so we get enough names for the testing stuff.

The `test` command should run all of the tests in the current directory.

```unison
test1 : [Result]
test1 = [Ok "test1"]
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test1 : [Result]

```
```ucm
.> test

  ✅  

  

  

    New test results:
  
  ◉ test1   test1
  
  ✅ 1 test(s) passing
  
  Tip: Use view test1 to view the source of a test.

```
Tests should be cached if unchanged.

```ucm
.> test

  Cached test results (`help testcache` to learn more)
  
  ◉ test1   test1
  
  ✅ 1 test(s) passing
  
  Tip: Use view test1 to view the source of a test.

```
`test` won't descend into the `lib` namespace, but `test.all` will.

```unison
testInLib : [Result]
testInLib = [Ok "testInLib"]
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testInLib : [Result]

```
```ucm
.> test

  Cached test results (`help testcache` to learn more)
  
  ◉ test1   test1
  
  ✅ 1 test(s) passing
  
  Tip: Use view test1 to view the source of a test.

.> test.all

    
    Cached test results (`help testcache` to learn more)
    
    ◉ test1   test1
    
    ✅ 1 test(s) passing
    
    ✅  

  

  

    New test results:
  
  ◉ lib.testInLib   testInLib
  
  ✅ 1 test(s) passing
  
  Tip: Use view lib.testInLib to view the source of a test.

```
`test` WILL run tests within `lib` if ucm is cd'd inside.

```ucm
.lib> test

  Cached test results (`help testcache` to learn more)
  
  ◉ testInLib   testInLib
  
  ✅ 1 test(s) passing
  
  Tip: Use view testInLib to view the source of a test.

```
