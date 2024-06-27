```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
```

```ucm
scratch/main> add
scratch/main> alias.term Foo.Bar Stray.BarAlias
```

```unison
unique type Foo = Bar Nat Nat
```

Bug: we leave `Stray.BarAlias` in the namespace with a nameless decl.

```ucm
scratch/main> update
scratch/main> find.verbose
```
