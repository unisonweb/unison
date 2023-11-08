```ucm
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
structural type Foo = Bar Nat
```

```ucm
.> add
```

```unison
structural type Foo = Bar Nat Nat
```

```ucm
.> update
.> view Foo
.> find.verbose
```
