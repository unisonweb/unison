```ucm:hide
.> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n+1)
```

```ucm
.> add
```

```unison
unique type Foo = Bar Nat Nat
```

```ucm:error
.> update
```
