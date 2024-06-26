```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison:hide
foo = 1
lib.foo = 2
lib.bar = 3
cat.foo = 4
cat.lib.foo = 5
cat.lib.bar = 6
somewhere.bar = 7
```

```ucm:hide
scratch/main> add
```

```ucm
scratch/main> find foo
scratch/main> view 1
scratch/main> find.all foo
scratch/main> view 1
```

```ucm
scratch/main> find-in cat foo
scratch/main> view 1
scratch/main> find-in.all cat foo
scratch/main> view 1
```

Finding within a namespace

```ucm
scratch/main> find bar
-- Shows UUIDs
-- scratch/main> find.global bar
scratch/main> find-in somewhere bar
```

```ucm:error
scratch/main> find baz
```

```ucm:error
scratch/main> find.global notHere
```
