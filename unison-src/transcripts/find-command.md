```unison
foo = 1
lib.foo = 2
lib.bar = 3
```

```ucm
.> add
```

```ucm
.> find foo
```

```ucm
.somewhere> find.global foo
```

```ucm
.> find bar
```

```ucm:error
.> find baz
```

```ucm:error
.> find.global nothere
```
