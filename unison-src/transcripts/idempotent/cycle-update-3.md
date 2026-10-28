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

  I've switched you to a temporary branch, update-main, and put
  the definitions that need attention in update-d59b4b8b.u.

  To finish the update:

    1. Edit update-d59b4b8b.u until it compiles. Fix each
       definition, or delete the ones you no longer want:
       deleting a definition from the file deletes it from the
       branch.
    2. Run `load update-d59b4b8b.u` to check the file. Repeat
       steps 1 and 2 until it loads with no errors.
    3. Run `update` (without arguments, from this branch). It
       saves the file's definitions, merges update-main into
       main and deletes the temporary branch.

  Or, to abandon the update instead, run `cancel` to delete the
  temporary branch and switch back to main.
```

``` unison :added-by-ucm update-d59b4b8b.u
ping : Nat
ping = 3

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

pong : 'Nat
pong _ =
  use Nat +
  ping() + 2

```
