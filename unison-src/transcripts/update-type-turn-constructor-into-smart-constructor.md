```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo = Bar Nat

makeFoo : Nat -> Foo
makeFoo n = Bar (n+10)
```

```ucm
.> add
```

```unison
unique type Foo = internal.Bar Nat

Foo.Bar : Nat -> Foo
Foo.Bar n = internal.Bar n
```

```ucm
.> update
.> view Foo
.> find.verbose
```
