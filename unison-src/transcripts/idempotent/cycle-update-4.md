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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      ping : 'Nat
      pong : 'Nat
```

``` ucm
scratch/main> add

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

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      clang : 'Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : 'Nat
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view ping pong clang

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
