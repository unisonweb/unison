```ucm:hide
scratch/main> builtins.merge
scratch/main> move builtin lib.builtin
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

```ucm
scratch/main somewhere> find bar
scratch/main somewhere> find.global bar
```

```ucm
scratch/main> find bar
scratch/main> find-in somewhere bar
```

```ucm:error
scratch/main> find baz
```

```ucm:error
scratch/main> find.global notHere
```
