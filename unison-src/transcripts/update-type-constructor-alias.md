```ucm:hide
.> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
```

```ucm
.> add
.> alias.term Foo.Bar Foo.BarAlias
```

```unison
unique type Foo = Bar Nat Nat
```

Bug: we leave `Foo.BarAlias` in the namespace with a nameless decl.

```ucm
.> update
.> find.verbose
```
