```ucm:hide
.> builtins.merge lib.builtin
```

```unison
unique type Foo = Nat
```

```ucm
.> add
```

```unison
unique type Foo = { bar : Nat }
```

```ucm
.> update
.> view Foo
.> find.verbose
```
