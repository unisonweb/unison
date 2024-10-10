`update` properly discovers and establishes new cycles.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
ping : 'Nat
ping _ = 1

pong : 'Nat
pong _ = !ping + 2
```

``` ucm
scratch/main> add
```

``` unison
ping : 'Nat
ping _ = !clang + 1

clang : 'Nat
clang _ = !pong + 3
```

``` ucm
scratch/main> update.old ping
scratch/main> view ping pong clang
```
