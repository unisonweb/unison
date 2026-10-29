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

  You're now on a temporary branch, update-main, and the
  definitions to fix are in update-main-a84dea19.u.

  Edit the file (deleting a definition from it deletes it from
  the branch), then run:

      scratch/update-main> load update-main-a84dea19.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      scratch/update-main> update

  `update` merges back into main and deletes the temporary
  branch. To abandon the update instead, run `cancel`.
```

``` unison :added-by-ucm update-main-a84dea19.u
foo n = "hello, world!"

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

test> mynamespace.foo.test =
  n = 2
  if foo n == 2 then [Ok "passed"] else [Fail "wat"]

```
