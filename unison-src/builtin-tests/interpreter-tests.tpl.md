```ucm:hide:error
.> delete.project nothing
.> delete.project runtime-tests
.> clone ${runtime_tests_version} runtime-tests/selected
```

```ucm
runtime-tests/selected> run tests
runtime-tests/selected> run tests.interpreter.only
```
