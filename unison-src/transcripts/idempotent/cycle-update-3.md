Update a member of a cycle with a type-changing update, thus severing the cycle.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
ping : 'Nat
ping _ = !pong + 1

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
ping : Nat
ping = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : Nat
```

``` ucm :error
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.
```

``` unison :added-by-ucm scratch.u
ping : Nat
ping = 3

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

pong : 'Nat
pong _ =
  use Nat +
  ping() + 2

```
