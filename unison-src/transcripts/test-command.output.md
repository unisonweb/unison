Merge builtins so we get enough names for the testing stuff.

The `test` command should run all of the tests in the current directory.

```unison
test1 : [Result]
test1 = [Ok "test1"]

foo.test2 : [Result]
foo.test2 = [Ok "test2"]
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo.test2 : [Result]
      test1     : [Result]

```
```ucm
.> test

  ✅  

  

  

  

  

    New test results:
  
    1. foo.test2   ◉ test2
    2. test1       ◉ test1
  
  ✅ 2 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

```
Tests should be cached if unchanged.

```ucm
.> test

  Cached test results (`help testcache` to learn more)
  
    1. foo.test2   ◉ test2
    2. test1       ◉ test1
  
  ✅ 2 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

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
  
    1. foo.test2   ◉ test2
    2. test1       ◉ test1
  
  ✅ 2 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

.> test.all

    
    Cached test results (`help testcache` to learn more)
    
      1. foo.test2   ◉ test2
      2. test1       ◉ test1
    
    ✅ 2 test(s) passing
    
    ✅  

  

  

    New test results:
  
    1. lib.testInLib   ◉ testInLib
  
  ✅ 1 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

```
`test` WILL run tests within `lib` if ucm is cd'd inside.

```ucm
.lib> test

  Cached test results (`help testcache` to learn more)
  
    1. testInLib   ◉ testInLib
  
  ✅ 1 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

```
`test` can be given a relative path, in which case it will only run tests found somewhere in that namespace.

```ucm
.> test foo

  Cached test results (`help testcache` to learn more)
  
    1. foo.test2   ◉ test2
  
  ✅ 1 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

```
