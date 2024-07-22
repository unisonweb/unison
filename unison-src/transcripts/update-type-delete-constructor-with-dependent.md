```ucm:hide
scratch/main> builtins.merge lib.builtin
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
scratch/main> add
```

```unison
unique type Foo
  = Bar Nat
```

```ucm:error
scratch/main> update
```
