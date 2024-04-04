The runtime tests are hosted at https://share.unison-lang.org/@unison/runtime-tests/

If you want to add or update tests, you can create a branch of that project, and update the `runtime_tests_version` line in `jit-tests.sh` and `CI.yaml`

Before merging the PR on Github, we'll merge your branch on Share and restore `runtime_tests_version` to /main or maybe a release.

```ucm:hide:error
.> delete.project dont-delete
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
