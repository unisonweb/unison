The `branch.diff` command shows a "diff preview" between two branches - a high-level overview of adds, updates, and
deletes, for each branch, since their LCA.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
foo = 17
bar = foo + foo
baz = 18
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  + baz : Nat
  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/main> switch /alice

scratch/alice> delete bar

  I deleted these terms:

    1. bar

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` unison
foo = 19
qux = 20
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + qux : Nat
  ~ foo : Nat

  + (added), ~ (modified)

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
baz = 21
bar = foo + foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ bar : Nat
  ~ baz : Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/bob> branch.diff /alice /bob

  Changes on /alice:

  + qux : Nat
  ~ foo : Nat
  - bar : Nat

  Changes on /bob:

  ~ bar : Nat
  ~ baz : Nat

  + (added), ~ (modified), - (deleted)
```

``` ucm :hide
scratch/main> project.delete scratch
```
