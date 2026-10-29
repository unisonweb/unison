``` ucm
> builtins.merge

  Done.
```

``` unison
foo : Nat
foo = 5

bar : Nat
bar = foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo : Int
foo = +5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : Int

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
  definitions to fix are in update-main-f1f0e905.u.

  Edit the file (deleting a definition from it deletes it from
  the branch), then run:

      scratch/update-main> load update-main-f1f0e905.u

  Repeat the edit and `load` until the file loads with no
  errors, then run:

      scratch/update-main> update

  `update` merges back into main and deletes the temporary
  branch. To abandon the update instead, run `cancel`.
```

``` unison :added-by-ucm update-main-f1f0e905.u
foo : Int
foo = +5

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

bar : Nat
bar =
  use Nat +
  foo + 10

```
