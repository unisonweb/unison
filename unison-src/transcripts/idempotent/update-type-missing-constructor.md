``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
```

``` ucm
scratch/main> add
scratch/main> delete.term Foo.Bar
```

Now we've set up a situation where the original constructor missing.

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :error
scratch/main> view Foo
scratch/main> update
```
