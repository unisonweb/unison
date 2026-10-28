``` ucm :hide
> builtins.merge
```

Given a test that depends on another definition,

``` unison :hide
foo n = n + 1

test> mynamespace.foo.test =
  n = 2
  if (foo n) == 2 then [ Ok "passed" ] else [ Fail "wat" ]
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

if we change the type of the dependency, the test should show in the scratch file as a test watch.

``` unison
foo n = "hello, world!"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : n -> Text

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
  the definitions that need attention in update-a498a0f7.u.

  To finish the update:

    1. Edit update-a498a0f7.u until it compiles. Fix each
    definition, or delete the ones you no longer want: deleting
    a definition from the file deletes it from the branch.
    2. Run `load update-a498a0f7.u` to check the file. Repeat
    steps 1 and 2 until it loads with no errors.
    3. Run `update` (without arguments, from this branch). It
    saves the file's definitions, merges update-main into main
    and deletes the temporary branch.

  Or, to abandon the update instead, run `cancel` to delete the
  temporary branch and switch back to main.
```

``` unison :added-by-ucm update-a498a0f7.u
foo n = "hello, world!"

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

test> mynamespace.foo.test =
  n = 2
  if foo n == 2 then [Ok "passed"] else [Fail "wat"]

```
