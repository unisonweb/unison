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

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
```

``` unison :added-by-ucm scratch.u
foo n = "hello, world!"

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

test> mynamespace.foo.test =
  n = 2
  if foo n == 2 then [Ok "passed"] else [Fail "wat"]

```
