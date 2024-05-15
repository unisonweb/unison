Merge builtins so we get enough names for the testing stuff.

```ucm:hide
.> builtins.merge
```

The `test` command should run all of the tests in the current directory.

```unison
test1 : [Result]
test1 = [Ok "test1"]

foo.test2 : [Result]
foo.test2 = [Ok "test2"]
```

```ucm:hide
.> add
```

```ucm
.> test
```

Tests should be cached if unchanged.

```ucm
.> test
```

`test` won't descend into the `lib` namespace, but `test.all` will.

```unison
testInLib : [Result]
testInLib = [Ok "testInLib"]
```

```ucm:hide
.lib> add
```

```ucm
.> test
.> test.all
```

`test` WILL run tests within `lib` if ucm is cd'd inside.

```ucm
.lib> test
```

`test` can be given a relative path, in which case it will only run tests found somewhere in that namespace.

```ucm
.> test foo
```
