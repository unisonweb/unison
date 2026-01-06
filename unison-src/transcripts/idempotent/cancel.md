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

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
```

``` unison :added-by-ucm scratch.u
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

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
```

``` unison :added-by-ucm scratch.u
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
  However, I've added the definitions that need attention to the
  top of scratch.u.

  When you're done, you can run

    update

  to merge your changes back into alice and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    cancel

  to delete the temporary branch and switch back to alice.
```

``` unison :added-by-ucm scratch.u
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
