```ucm:hide
.> builtins.merge lib.builtin
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

```ucm
.> update
.> view Foo
.> find.verbose
```
