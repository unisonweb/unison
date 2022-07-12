Merge builtins so we get enough names for the testing stuff.

The `test` command should run all of the tests in the current directory.

(Just so we don't have to pull in `.base` into this transcript, we make a fakey test just by giving it the right type,
and manually linking it to the builtin `isTest` value).

```unison
test1 : [Result]
test1 = [Ok "test1"]
```

```ucm

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
`test` won't descend into the `lib` namespace, but `test.all` will.

```unison
test2 : [Result]
test2 = [Ok "test2"]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test2 : [Result]

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
  
  ◉ lib.test2   test2
  
  ✅ 1 test(s) passing
  
  Tip: Use view lib.test2 to view the source of a test.

```
`test` will descend into namespaces named `lib` if they aren't at the top-level, though.

```unison
test3 : [Result]
test3 = [Ok "test3"]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test3 : [Result]

```
```ucm
.> test

    
    Cached test results (`help testcache` to learn more)
    
    ◉ test1   test1
    
    ✅ 1 test(s) passing
    
    ✅  

  

  

    New test results:
  
  ◉ hello.lib.test3   test3
  
  ✅ 1 test(s) passing
  
  Tip: Use view hello.lib.test3 to view the source of a test.

```
