```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo = Bar Nat
```

```ucm
.> add
.> alias.term Foo.Bar Stray.BarAlias
```

```unison
unique type Foo = Bar Nat Nat
```

Bug: we leave `Stray.BarAlias` in the namespace with a nameless decl.

```ucm
.> update
.> find.verbose
```
