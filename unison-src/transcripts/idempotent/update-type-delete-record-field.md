``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = { bar : Nat, baz : Int }
```

``` ucm
scratch/main> add
```

``` unison
unique type Foo = { bar : Nat }
```

We want the field accessors to go away; but for now they are here, causing the update to fail.

``` ucm :error
scratch/main> update
scratch/main> view Foo
scratch/main> find.verbose
```
