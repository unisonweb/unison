`update` properly discovers and establishes new cycles.

``` ucm :hide
> builtins.merge
```

``` unison
ping : 'Nat
ping _ = 1

pong : 'Nat
pong _ = !ping + 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ping : 'Nat
  + pong : 'Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
ping : 'Nat
ping _ = !clang + 1

clang : 'Nat
clang _ = !pong + 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + clang : 'Nat
  ~ ping : 'Nat

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

> view ping pong clang

  clang : 'Nat
  clang _ =
    use Nat +
    pong() + 3

  ping : 'Nat
  ping _ =
    use Nat +
    clang() + 1

  pong : 'Nat
  pong _ =
    use Nat +
    ping() + 2
```
