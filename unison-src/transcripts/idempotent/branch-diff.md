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

If two branches are identical, we just get a textual message (no diff).

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` ucm
scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/main> branch.diff /main /alice

  Those branches are the same.
```

``` ucm :hide
scratch/main> project.delete scratch
```

`branch.diff` Also shows changes in `lib.*`:

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
lib.foo.foo = 17
lib.bar.bar = 18
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lib.bar.bar : Nat
  + lib.foo.foo : Nat

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
```

``` unison
lib.foo.foo = 18
lib.baz.baz = 19
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lib.baz.baz : Nat
  ~ lib.foo.foo : Nat
      (also named lib.bar.bar)

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/alice> delete.namespace lib.bar

  Done.

scratch/alice> branch.diff /main /alice

  Changes on /alice:

  + lib.baz
  ~ lib.foo
  - lib.bar

  + (added), ~ (modified), - (deleted)
```

``` ucm :hide
scratch/main> project.delete scratch
```

Currently (and temporarily), `branch.diff` doesn't work if the two branches don't share any history.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` ucm
scratch/main> branch.create-empty topic

  Done. I've created an empty branch scratch/topic.

  Tip: Use `merge /somebranch` to initialize this branch.

scratch/main> branch.diff /main /topic

  Sorry, I can't yet compute the difference between branches that don't have any history in common.
```

``` ucm :hide
scratch/main> project.delete scratch
```
