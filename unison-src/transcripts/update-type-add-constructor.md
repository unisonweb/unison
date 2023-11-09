```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo
  = Bar Nat
```

```ucm
.> add
```

```unison
unique type Foo
  = Bar Nat
  | Baz Nat Nat
```

```ucm
.> update
.> view Foo
.> find.verbose
```
