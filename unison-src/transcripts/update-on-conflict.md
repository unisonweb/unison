# Update on conflict

Conflicted definitions prevent `update` from succeeding.

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

``` unison
x = 1
temp = 2
```

``` ucm
scratch/main> add
scratch/main> debug.alias.term.force temp x
scratch/main> delete.term temp
```

``` unison
x = 3
```

``` ucm :error
scratch/main> update
```
