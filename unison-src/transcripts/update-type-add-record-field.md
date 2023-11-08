```ucm
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
structural type Foo = { bar : Nat }
```

```ucm
.> add
```

```unison
structural type Foo = { bar : Nat, baz : Int }
```

```ucm
.> update
.> view Foo
.> find.verbose
```

