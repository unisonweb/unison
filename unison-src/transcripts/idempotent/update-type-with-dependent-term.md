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
  definitions to fix are in update-main-b46cd0bf.u.

  Edit the file (deleting a definition from it deletes it from
  the branch), then run:

      scratch/update-main> load update-main-b46cd0bf.u

  Repeat the edit and `load` until the file loads with no
  errors, then run:

      scratch/update-main> update

  `update` merges back into main and deletes the temporary
  branch. To abandon the update instead, run `cancel`.
```

``` unison :added-by-ucm update-main-b46cd0bf.u
type Foo = Bar Nat Nat

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n Nat.+ 1)

```
