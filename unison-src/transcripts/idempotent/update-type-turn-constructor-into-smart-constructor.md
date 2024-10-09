``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat

makeFoo : Nat -> Foo
makeFoo n = Bar (n+10)
```

``` ucm
scratch/main> add
```

``` unison
unique type Foo = internal.Bar Nat

Foo.Bar : Nat -> Foo
Foo.Bar n = internal.Bar n
```

``` ucm
scratch/main> update
scratch/main> view Foo
scratch/main> find.verbose
```
