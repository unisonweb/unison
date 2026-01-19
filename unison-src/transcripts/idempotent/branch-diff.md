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
foo = 17
bar = 18
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

scratch/main> move.term foo lib.foo.foo

  Done.

scratch/main> move.term bar lib.bar.bar

  Done.

scratch/main> branch alice

  Done. I've created the alice branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

scratch/alice> delete.force lib.foo.foo

  I deleted these terms:

    1. lib.foo.foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

``` unison
foo = 18
baz = 19
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + baz : Nat
  + foo : Nat
      (also named lib.bar.bar)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/alice> move.term foo lib.foo.foo

  Done.

scratch/alice> move.term baz lib.baz.baz

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

If two branches don't share any history, `branch.diff` treats the first argument as the LCA.

``` unison
main = "main"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + main : ##Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> branch.create-empty topic

  Done. I've created an empty branch scratch/topic.

  Tip: Use `merge /somebranch` to initialize this branch.
```

``` unison
topic = "topic"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + topic : ##Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/topic> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/topic> branch.diff /main /topic

  Changes on /topic:

  + topic : ##Text
  - main : ##Text

  + (added), - (deleted)

scratch/topic> branch.diff /topic /main

  Changes on /main:

  + main : ##Text
  - topic : ##Text

  + (added), - (deleted)
```

``` ucm :hide
scratch/main> project.delete scratch
```

Libdep names are canonicalized, so differences in libdeps often register as propagated changes, not actual changes.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
thing = 17
foo = thing + thing
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo   : Nat
  + thing : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move thing lib.dep_1_0_0.thing

  Done.

scratch/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.
```

``` unison
thing = 18
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/topic> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/topic> move thing lib.dep_2_0_0.thing

  Done.

scratch/topic> upgrade dep_1_0_0 dep_2_0_0

  I upgraded dep_1_0_0 to dep_2_0_0.

scratch/topic> diff.branch /main /topic

  Changes on /topic:

  + lib.dep_2_0_0
  - lib.dep_1_0_0

  + (added), - (deleted)
```

``` ucm :hide
scratch/main> project.delete scratch
```
