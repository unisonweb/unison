Merge builtins so we get enough names for the testing stuff.

``` ucm :hide
> builtins.merge
```

The `test` command should run all of the tests in the current directory.

``` unison
test1 : [Result]
test1 = [Ok "test1"]

foo.test2 : [Result]
foo.test2 = [Ok "test2"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo.test2 : [Result]
  + test1     : [Result]

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

``` ucm
> test

  ✅  





    New test results:

    1. foo.test2   ◉ test2
    2. test1       ◉ test1

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

Tests should be cached if unchanged.

``` ucm
> test

  Cached test results (`help testcache` to learn more)

    1. foo.test2   ◉ test2
    2. test1       ◉ test1

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`test` won't descend into the `lib` namespace, but `test.all` will.

``` unison
dep.testInLib : [Result]
dep.testInLib = [Ok "testInLib"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dep.testInLib : [Result]

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> move dep.testInLib lib.dep.testInLib

  Done.
```

``` ucm
> test

  Cached test results (`help testcache` to learn more)

    1. foo.test2   ◉ test2
    2. test1       ◉ test1

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.

> test.all

    
    Cached test results (`help testcache` to learn more)
    
      1. foo.test2   ◉ test2
      2. test1       ◉ test1
    
    ✅ 2 test(s) passing
    
    ✅  



    New test results:

    1. lib.dep.testInLib   ◉ testInLib

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`test` WILL run tests within `lib` if specified explicitly.

``` ucm
> test lib.dep

  Cached test results (`help testcache` to learn more)

    1. lib.dep.testInLib   ◉ testInLib

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`test` can be given a relative path, in which case it will only run tests found somewhere in that namespace.

``` ucm
> test foo

  Cached test results (`help testcache` to learn more)

    1. foo.test2   ◉ test2

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
