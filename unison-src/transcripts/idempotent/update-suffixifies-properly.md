``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
a.x.x.x.x = 100
b.x.x.x.x = 100
foo = 25
c.y.y.y.y = foo + 10
d.y.y.y.y = foo + 10
bar = a.x.x.x.x + c.y.y.y.y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + a.x.x.x.x : Nat
  + b.x.x.x.x : Nat
  + bar       : Nat
  + c.y.y.y.y : Nat
  + d.y.y.y.y : Nat
  + foo       : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo = +30
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
  definitions to fix are in update-main-84a6c46a.u.

  Edit the file (deleting a definition from it deletes it from
  the branch), then run:

      scratch/update-main> load update-main-84a6c46a.u

  Repeat the edit and `load` until the file loads with no
  errors, then run:

      scratch/update-main> update

  `update` merges back into main and deletes the temporary
  branch. To abandon the update instead, run `cancel`.
```

``` unison :added-by-ucm update-main-84a6c46a.u
foo = +30

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

bar : Nat
bar =
  use Nat +
  x + c.y.y.y.y

c.y.y.y.y : Nat
c.y.y.y.y =
  use Nat +
  foo + 10

d.y.y.y.y : Nat
d.y.y.y.y =
  use Nat +
  foo + 10

```
