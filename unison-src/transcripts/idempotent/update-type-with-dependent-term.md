``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n+1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  + incrFoo : Foo -> Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

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
  the definitions that need attention in update-c865a093.u.

  To finish the update:

    1. Edit update-c865a093.u until it compiles. Fix each
    definition, or delete the ones you no longer want: deleting
    a definition from the file deletes it from the branch.
    2. Run `load update-c865a093.u` to check the file. Repeat
    steps 1 and 2 until it loads with no errors.
    3. Run `update` (without arguments, from this branch). It
    saves the file's definitions, merges update-main into main
    and deletes the temporary branch.

  Or, to abandon the update instead, run `cancel` to delete the
  temporary branch and switch back to main.
```

``` unison :added-by-ucm update-c865a093.u
type Foo = Bar Nat Nat

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n Nat.+ 1)

```
