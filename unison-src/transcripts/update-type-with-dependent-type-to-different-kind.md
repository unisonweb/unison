```ucm
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
structural type Foo = Bar Nat
structural type Baz = Qux Foo
```

```ucm
.> add
```

```unison
structural type Foo a = Bar Nat a
```

```ucm:error
.> update
```
