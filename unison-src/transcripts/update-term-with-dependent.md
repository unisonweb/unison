```ucm
.> builtins.merge
```

```unison
foo : Nat
foo = 5

bar : Nat
bar = foo + 10
```

```ucm
.> add
```

```unison
foo : Nat
foo = 6
```

```ucm
.> update
.> view bar
```
