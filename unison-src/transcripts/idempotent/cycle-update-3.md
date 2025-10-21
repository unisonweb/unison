Update a member of a cycle with a type-changing update, thus severing the cycle.

``` ucm :hide
> builtins.merge
```

``` unison
ping : 'Nat
ping _ = !pong + 1

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
ping : Nat
ping = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ ping : Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  I couldn't complete the update, because some existing
  definitions would no longer typecheck.

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use`update`to merge them back intomain,or`cancel`if you change your mind.
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
