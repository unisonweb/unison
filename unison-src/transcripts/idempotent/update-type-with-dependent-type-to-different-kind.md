``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Baz
  + type Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
unique type Foo a = Bar Nat a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo a

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
  the definitions that need attention in update-dbfd7ab5.u.

  To finish the update:

    1. Edit update-dbfd7ab5.u until it compiles. Fix each
    definition, or delete the ones you no longer want: deleting
    a definition from the file deletes it from the branch.
    2. Run `load update-dbfd7ab5.u` to check the file. Repeat
    steps 1 and 2 until it loads with no errors.
    3. Run `update` (without arguments, from this branch). It
    saves the file's definitions, merges update-main into main
    and deletes the temporary branch.

  Or, to abandon the update instead, run `cancel` to delete the
  temporary branch and switch back to main.
```

``` unison :added-by-ucm update-dbfd7ab5.u
type Foo a = Bar Nat a

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

type Baz = Qux Foo

```
