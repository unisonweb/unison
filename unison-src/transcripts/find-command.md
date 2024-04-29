```ucm:hide
.> builtins.merge
.> move builtin lib.builtin
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
.> add
```

```ucm
.> find foo
.> view 1
.> find.all foo
.> view 1
```

```ucm
.> find-in cat foo
.> view 1
.> find-in.all cat foo
.> view 1
```

```ucm
.somewhere> find bar
.somewhere> find.global bar
```

```ucm
.> find bar
.> find-in somewhere bar
```

```ucm:error
.> find baz
```

```ucm:error
.> find.global notHere
```
