``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo
  = Bar Nat
  | Baz Nat Nat

foo : Foo -> Nat
foo = cases
  Bar n -> n
  Baz n m -> n + m
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  + foo : Foo -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
unique type Foo
  = Bar Nat
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
  definitions to fix are in update-main-d05cf7b6.u.

  Edit the file (deleting a definition from it deletes it from
  the branch), then run:

      scratch/update-main> load update-main-d05cf7b6.u

  Repeat the edit and `load` until the file loads with no
  errors, then run:

      scratch/update-main> update

  `update` merges back into main and deletes the temporary
  branch. To abandon the update instead, run `cancel`.
```

``` unison :added-by-ucm update-main-d05cf7b6.u
type Foo = Bar Nat

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

foo : Foo -> Nat
foo = cases
  Bar n   -> n
  Baz n m -> n Nat.+ m

```
