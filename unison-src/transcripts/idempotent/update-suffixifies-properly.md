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

  I've switched you to a temporary branch, update-main, and put
  the definitions that need attention in update-b8d06ddf.u.

  To finish the update:

    1. Edit update-b8d06ddf.u until it compiles. Fix each
       definition, or delete the ones you no longer want:
       deleting a definition from the file deletes it from the
       branch.
    2. Run `load update-b8d06ddf.u` to check the file. Repeat
       steps 1 and 2 until it loads with no errors.
    3. Run `update` (without arguments, from this branch). It
       saves the file's definitions, merges update-main into
       main and deletes the temporary branch.

  Or, to abandon the update instead, run `cancel` to delete the
  temporary branch and switch back to main.
```

``` unison :added-by-ucm update-b8d06ddf.u
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
