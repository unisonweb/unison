``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = { bar : Nat }
```

``` ucm
scratch/main> add
```

Bug: this no-op update should (of course) succeed.

``` ucm
scratch/main> update
```
