```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo
  = Bar Nat
  | Baz Nat Nat

foo : Foo -> Nat
foo = cases
  Bar n -> n
  Baz n m -> n + m
```

```ucm
.> add
```

```unison
unique type Foo
  = Bar Nat
```

```ucm:error
.> update
```
