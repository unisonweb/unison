```unison
foo = 1
lib.foo = 2
lib.bar = 3
foo.lib.qux = 4
```

```ucm
.> add
```

```ucm
.> find foo
```

```ucm:error
.> find bar
```

```ucm:error
.> find baz
```

```ucm
.> find qux
```
