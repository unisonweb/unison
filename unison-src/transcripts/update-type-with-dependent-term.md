```ucm
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
structural type Foo = Bar Nat

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n+1)
```

```ucm
.> add
```

```unison
structural type Foo = Bar Nat Nat
```

```ucm:error
.> update
```
