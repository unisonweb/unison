See [this ticket](https://github.com/unisonweb/unison/issues/873); the point being, this shouldn't crash the runtime. :)

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
(-) = builtin.Nat.sub
```

``` ucm
scratch/main> add
```

``` unison
baz x = x - 1
```
