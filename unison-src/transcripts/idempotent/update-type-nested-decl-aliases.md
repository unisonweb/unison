``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat

structural type A.B = OneAlias Foo
structural type A = B.TheOtherAlias Foo
```

``` ucm
scratch/main> add
```

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :error
scratch/main> update
```
