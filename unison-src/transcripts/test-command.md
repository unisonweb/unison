Merge builtins so we get enough names for the testing stuff.

```ucm:hide
scratch/main> builtins.merge
```

The `test` command should run all of the tests in the current directory.

```unison
test1 : [Result]
test1 = [Ok "test1"]

foo.test2 : [Result]
foo.test2 = [Ok "test2"]
```

```ucm:hide
scratch/main> add
```

```ucm
scratch/main> test
```

Tests should be cached if unchanged.

```ucm
scratch/main> test
```

`test` won't descend into the `lib` namespace, but `test.all` will.

```unison
lib.dep.testInLib : [Result]
lib.dep.testInLib = [Ok "testInLib"]
```

```ucm:hide
scratch/main> add
```

```ucm
scratch/main> test
scratch/main> test.all
```

`test` WILL run tests within `lib` if specified explicitly.

```ucm
scratch/main> test lib.dep
```

`test` can be given a relative path, in which case it will only run tests found somewhere in that namespace.

```ucm
scratch/main> test foo
```
