```ucm:hide:error
.> delete.project nothing
.> delete.project runtime-tests
.> clone ${runtime_tests_version} runtime-tests/selected
```

```ucm
runtime-tests/selected> run.native tests
runtime-tests/selected> run.native tests.jit.only
```

Per Dan:
It's testing a flaw in how we were sending code from a scratch file to the native runtime, when that happened multiple times.
Related to the verifiable refs and recursive functions.
```unison
foo = do
  go : Nat ->{Exception} ()
  go = cases
    0 -> ()
    n -> go (decrement n)
  go 1000
```

```ucm
.> run.native foo
.> run.native foo
```
