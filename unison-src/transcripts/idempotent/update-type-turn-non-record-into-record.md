``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Nat
```

``` ucm
scratch/main> add
```

``` unison
unique type Foo = { bar : Nat }
```

``` ucm
scratch/main> update
scratch/main> view Foo
scratch/main> find.verbose
```
