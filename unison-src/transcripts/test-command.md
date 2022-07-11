Merge builtins so we get enough names for the testing stuff.

```ucm:hide
.> builtins.merge
```

The `test` command should run all of the tests in the current directory.

(Just so we don't have to pull in `.base` into this transcript, we make a fakey test just by giving it the right type,
and manually linking it to the builtin `isTest` value).

```unison
test1 : [Result]
test1 = [Ok "test1"]
```

```ucm:hide
.> add
.> link .builtin.metadata.isTest test1
```

```ucm
.> test
```

`test` won't descend into the `lib` namespace, but `test.all` will.

```unison
test2 : [Result]
test2 = [Ok "test2"]
```

```ucm:hide
.lib> add
.lib> link .builtin.metadata.isTest test2
```

```ucm
.> test
.> test.all
```
