```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo = { bar : Nat, baz : Int }
```

```ucm
.> add
```

```unison
unique type Foo = { bar : Nat }
```

We want the field accessors to go away; but for now they are here, causing the update to fail.

```ucm:error
.> update
.> view Foo
.> find.verbose
```
