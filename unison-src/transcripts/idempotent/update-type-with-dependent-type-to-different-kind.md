``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
```

``` ucm
scratch/main> add
```

``` unison
unique type Foo a = Bar Nat a
```

``` ucm :error
scratch/main> update
```
