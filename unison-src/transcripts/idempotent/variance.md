Tests some less trivial variance cases.

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison
act1 : [(Nat, '{IO} Nat)]
act1 = [(5, '5)]

act2 : [(Nat, '{Exception} Nat)]
act2 = [(5, '5)]

act12 = act1 ++ act2

act1or2 = if true then act1 else act2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + act1    : [(Nat, '{IO} Nat)]
  + act12   : [(Nat, '{IO, Exception} Nat)]
  + act1or2 : [(Nat, '{IO, Exception} Nat)]
  + act2    : [(Nat, '{Exception} Nat)]

  Run `update` to apply these changes to your codebase.
```
