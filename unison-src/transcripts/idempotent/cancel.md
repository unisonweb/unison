# The `cancel` command.

## `cancel` cancels an in-progress update.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo = 17
bar = foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo = +17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : Int

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  I couldn't complete the update, because some existing
  definitions would no longer typecheck.

  I've switched you to a temporary branch, update-main, and put
  the definitions that need attention in update-00b86312.u.

  To finish the update:

    1. Edit update-00b86312.u until it compiles. Fix each
    definition, or delete the ones you no longer want: deleting
    a definition from the file deletes it from the branch.
    2. Run `load update-00b86312.u` to check the file. Repeat
    steps 1 and 2 until it loads with no errors.
    3. Run `update` (without arguments, from this branch). It
    saves the file's definitions, merges update-main into main
    and deletes the temporary branch.

  Or, to abandon the update instead, run `cancel` to delete the
  temporary branch and switch back to main.
```

``` unison :added-by-ucm update-00b86312.u
foo = +17

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

bar : Nat
bar =
  use Nat +
  foo + foo

```

``` ucm
scratch/update-main> cancel

scratch/main> branches

       Branch   Remote branch
  1.   main     
```

``` ucm :hide
scratch/main> project.delete scratch
```

## `cancel` cancels an in-progress upgrade.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo1 = 17
foo2 = +17
bar = foo1 + foo1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar  : Nat
  + foo1 : Nat
  + foo2 : Int

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move foo1 lib.old.foo

  Done.

scratch/main> move foo2 lib.new.foo

  Done.
```

``` ucm :error
scratch/main> upgrade old new

  I couldn't automatically upgrade old to new.

  I've switched you to a temporary branch, upgrade-old-to-new,
  and put the definitions that need attention in
  upgrade-e3493e45.u.

  To finish the upgrade:

    1. Edit upgrade-e3493e45.u until it compiles. Fix each
    definition, or delete the ones you no longer want: deleting
    a definition from the file deletes it from the branch.
    2. Run `load upgrade-e3493e45.u` to check the file. Repeat
    steps 1 and 2 until it loads with no errors.
    3. Run `update` (without arguments, from this branch). It
    saves the file's definitions, merges upgrade-old-to-new into
    main and deletes the temporary branch.

  Or, to abandon the upgrade instead, run `cancel` to delete the
  temporary branch and switch back to main.
```

``` unison :added-by-ucm upgrade-e3493e45.u
-- The definitions below no longer typecheck after upgrading.
-- Please fix the errors, then run `update`.

bar : Nat
bar =
  use Nat +
  foo + foo

```

``` ucm
scratch/upgrade-old-to-new> cancel

scratch/main> branches

       Branch   Remote branch
  1.   main     
```

``` ucm :hide
scratch/main> project.delete scratch
```

## `cancel` cancels an in-progress merge.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` ucm
scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.
```

``` unison
foo = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/alice> switch /main

scratch/main> branch bob

  Done. I've created the bob branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.
```

``` unison
foo = 18
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/bob> switch /alice
```

``` ucm :error
scratch/alice> merge /bob

  Loading namespaces...

  Computing diff...

  Loading definitions...

  Computing merge...

  I couldn't automatically merge scratch/bob into scratch/alice.

  I've switched you to a temporary branch, merge-bob-into-alice,
  and put the definitions that need attention in
  merge-871a0464.u.

  To finish the merge:

    1. Edit merge-871a0464.u until it compiles. Fix each
    definition, or delete the ones you no longer want: deleting
    a definition from the file deletes it from the branch.
    2. Run `load merge-871a0464.u` to check the file. Repeat
    steps 1 and 2 until it loads with no errors.
    3. Run `update` (without arguments, from this branch). It
    saves the file's definitions, merges merge-bob-into-alice
    into alice and deletes the temporary branch.

  Or, to abandon the merge instead, run `cancel` to delete the
  temporary branch and switch back to alice.
```

``` unison :added-by-ucm merge-871a0464.u
-- scratch/alice
foo : Nat
foo = 17

-- scratch/bob
foo : Nat
foo = 18

```

``` ucm
scratch/merge-bob-into-alice> cancel

scratch/alice> branches

       Branch   Remote branch
  1.   alice    
  2.   bob      
  3.   main     
```

``` ucm :hide
scratch/main> project.delete scratch
```

## `cancel` doesn't do anything when run on a non-update, non-upgrade, non-merge branch.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` ucm
scratch/main> cancel

  There's no merge, update, or upgrade in progress.
```

``` ucm :hide
scratch/main> project.delete scratch
```
