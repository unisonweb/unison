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

  You're now on a temporary branch, update-main, and the
  definitions to fix are in update-main-663cfccc.u.

  Edit the file, then run:

      scratch/update-main> load update-main-663cfccc.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      scratch/update-main> update

  `update` saves the file's definitions into main, removes any
  definitions you deleted from the file, and deletes the
  temporary branch. To abandon the update instead, run `cancel`.
```

``` unison :added-by-ucm update-main-663cfccc.u
ping : Nat
ping = 3

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

pong : 'Nat
pong _ =
  use Nat +
  ping() + 2

```
