``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n+1)

other : Nat
other = 1

usesOther : Nat
usesOther = other + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  + incrFoo   : Foo -> Foo
  + other     : Nat
  + usesOther : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

A change to `Foo` breaks `incrFoo`, so `update` can't finish.

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
  definitions to fix are in update-main-90f1b725.u.

  Edit the file, then run:

      scratch/update-main> load update-main-90f1b725.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      scratch/update-main> update

  `update` saves the file's definitions into main, removes any
  definitions you deleted from the file, and deletes the
  temporary branch. To abandon the update instead, run `cancel`.

  Note: Definitions listed in the file don't exist in the branch
  until a successful `update`.
```

``` unison :added-by-ucm update-main-90f1b725.u
type Foo = Bar Nat Nat

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

incrFoo : Foo -> Foo
incrFoo = cases Bar n -> Bar (n Nat.+ 1)

```

You are now on a temporary branch. The definitions in the file are not in it:

``` ucm
> branches

       Branch        Remote branch
  1.   main          
  2.   update-main   

> view other

  other : Nat
  other = 1
```

``` ucm :error
> view Foo

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    Foo
```

``` ucm :error
> view incrFoo

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    incrFoo
```

Fixing `incrFoo` and also changing `other`, which has a dependent that is not in the file, so `update` can't finish again:

``` unison
type Foo = Bar Nat Nat

incrFoo : Foo -> Foo
incrFoo = cases Bar n m -> Bar (n+1) m

other : Text
other = "hello"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ incrFoo : Foo -> Foo
  ~ other   : Text

  (and 1 unchanged term)

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Typechecking failed. I've updated scratch.u with the
  definitions that still need fixing.

  Edit the file, then run:

      scratch/update-main> load scratch.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      scratch/update-main> update

  To abandon the update instead, run `cancel`.
```

``` unison :added-by-ucm scratch.u
type Foo = Bar Nat Nat

incrFoo : Foo -> Foo
incrFoo = cases Foo.Bar n m -> Foo.Bar (n Nat.+ 1) m

other : Text
other = "hello"

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

usesOther : Nat
usesOther =
  use Nat +
  other + 1

```
