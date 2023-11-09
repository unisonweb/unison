```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
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
