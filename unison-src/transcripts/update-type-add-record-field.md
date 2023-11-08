```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo = { bar : Nat }
```

```ucm
.> add
```

```unison
unique type Foo = { bar : Nat, baz : Int }
```

```ucm:error
.> update
.> view Foo
.> find.verbose
```
