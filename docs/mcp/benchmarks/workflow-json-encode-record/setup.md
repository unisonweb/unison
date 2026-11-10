Starting codebase: `scratch/main` with `@unison/base` and a `Person` record, nothing else. The worker has to find a JSON library on Unison Share and install it, so it needs the network.

``` unison
unique type Person = { name : Text, age : Nat }
```

``` ucm
scratch/main> add
```
