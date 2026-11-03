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

  You're now on a temporary branch, update-main, and the
  definitions to fix are in update-main-306492da.u.

  Edit the file, then run:

      scratch/update-main> load update-main-306492da.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      scratch/update-main> update

  `update` saves the file's definitions into main, removes any
  definitions you deleted from the file, and deletes the
  temporary branch. To abandon the update instead, run `cancel`.

  Note: Definitions listed in the file don't exist in the branch
  until a successful `update`.
```

``` unison :added-by-ucm update-main-306492da.u
type Foo = Bar Nat Nat

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n Nat.+ 1)

```
