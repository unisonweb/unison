```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
```

```ucm
.> add
```

```unison
unique type Foo a = Bar Nat a
```

```ucm:error
.> update
```
