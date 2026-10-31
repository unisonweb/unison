# What a failed update, delete, upgrade or merge leaves behind

Each of these puts you on a temporary branch, with the definitions that need fixing in a file. The definitions in the
file are not on the branch (`update` is what saves them), but everything else is. The messages say so, so this checks
it.

## update

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

## delete

``` ucm :hide
deleteproj/main> builtins.merge lib.builtin
```

``` unison
a = 1
b = a + 1
other = 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + a     : Nat
  + b     : Nat
  + other : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
deleteproj/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

`b` depends on `a`, so `delete a` can't finish. `b` goes in the file, and is not on the branch. `other` is.

``` ucm :error
deleteproj/main> delete a

  I couldn't complete the delete, because some definitions are
  still in use.

  You're now on a temporary branch, update-main, and the
  definitions to fix are in delete-main-6120c455.u.

  Edit the file, then run:

      deleteproj/update-main> load delete-main-6120c455.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      deleteproj/update-main> update

  `update` saves the file's definitions into main, removes any
  definitions you deleted from the file, and deletes the
  temporary branch. To abandon the delete instead, run `cancel`.

  Note: Definitions listed in the file don't exist in the branch
  until a successful `update`.
```

``` unison :added-by-ucm delete-main-6120c455.u
-- The definitions below depend on the deleted definitions.
-- Please fix the errors and run `update`.

b : Nat
b =
  use Nat +
  a + 1

```

``` ucm
deleteproj/update-main> view other

  other : Nat
  other = 5
```

``` ucm :error
deleteproj/update-main> view b

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    b
```

## upgrade

``` ucm :hide
upgradeproj/main> builtins.merge lib.builtin
```

``` unison
old.foo = 17
new.foo = +18
thingy = old.foo + 10
unrelated = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + new.foo   : Int
  + old.foo   : Nat
  + thingy    : Nat
  + unrelated : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
upgradeproj/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

upgradeproj/main> move old.foo lib.old.foo

  Done.

upgradeproj/main> move new.foo lib.new.foo

  Done.
```

`thingy` doesn't typecheck against the new `foo`, so it goes in the file, and is not on the branch. `unrelated` is.

``` ucm :error
upgradeproj/main> upgrade old new

  I couldn't automatically upgrade old to new.

  You're now on a temporary branch, upgrade-old-to-new, and the
  definitions to fix are in upgrade-old-to-new-aec4f83d.u.

  Edit the file, then run:

      upgradeproj/upgrade-old-to-new> load upgrade-old-to-new-aec4f83d.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      upgradeproj/upgrade-old-to-new> update

  `update` saves the file's definitions into main, removes any
  definitions you deleted from the file, and deletes the
  temporary branch. To abandon the upgrade instead, run
  `cancel`.

  Note: Definitions listed in the file don't exist in the branch
  until a successful `update`.
```

``` unison :added-by-ucm upgrade-old-to-new-aec4f83d.u
-- The definitions below no longer typecheck after upgrading.
-- Please fix the errors, then run `update`.

thingy : Nat
thingy =
  use Nat +
  foo + 10

```

``` ucm
upgradeproj/upgrade-old-to-new> view unrelated

  unrelated : Nat
  unrelated = 3
```

``` ucm :error
upgradeproj/upgrade-old-to-new> view thingy

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    thingy
```

## merge

``` ucm :hide
mergeproj/main> builtins.merge lib.builtin
```

``` unison
foo = 1
user = foo + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo  : Nat
  + user : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
mergeproj/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

mergeproj/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

mergeproj/main> branch /bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.
```

``` unison
foo = 2
user = foo + 1
aliceOnly = 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + aliceOnly : Nat
  ~ foo  : Nat
  ~ user : Nat

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
mergeproj/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo = 3
user = foo + 1
bobOnly = 20
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bobOnly : Nat
  ~ foo  : Nat
  ~ user : Nat

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
mergeproj/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Alice and Bob both changed `foo`, so `foo` is in the file, and so is `user`, which depends on it. Neither is on the branch.
What each added, and merged cleanly, is.

``` ucm :error
mergeproj/alice> merge /bob

  Loading namespaces...

  Computing diff...

  Loading definitions...

  Computing merge...

  I couldn't automatically merge mergeproj/bob into
  mergeproj/alice.

  You're now on a temporary branch, merge-bob-into-alice, and
  the definitions to fix are in merge-bob-into-alice-368151a8.u.

  Edit the file, then run:

      mergeproj/merge-bob-into-alice> load merge-bob-into-alice-368151a8.u

  If `load` reports errors, fix them and run it again. When the
  file loads with no errors, run:

      mergeproj/merge-bob-into-alice> update

  `update` saves the file's definitions into alice, removes any
  definitions you deleted from the file, and deletes the
  temporary branch. To abandon the merge instead, run `cancel`.

  Note: Definitions listed in the file don't exist in the branch
  until a successful `update`.
```

``` unison :added-by-ucm merge-bob-into-alice-368151a8.u
-- mergeproj/alice
foo : Nat
foo = 2

-- mergeproj/bob
foo : Nat
foo = 3

-- ** The definitions below are not conflicted, but they each depend on one or more
-- conflicted definitions above.

user : Nat
user =
  use Nat +
  foo + 1

```

``` ucm
mergeproj/merge-bob-into-alice> view aliceOnly

  aliceOnly : Nat
  aliceOnly = 10

mergeproj/merge-bob-into-alice> view bobOnly

  bobOnly : Nat
  bobOnly = 20
```

``` ucm :error
mergeproj/merge-bob-into-alice> view foo

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    foo
```

``` ucm :error
mergeproj/merge-bob-into-alice> view user

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    user
```
