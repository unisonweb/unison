The `branch.diff` command shows a "diff preview" between two branches - a high-level overview of adds, updates, and
deletes, for each branch, since their LCA.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison :hide
foo = 17
bar = foo + foo
baz = 18
```

``` ucm :hide
scratch/main> update

scratch/main> branch alice

scratch/main> switch /alice

scratch/alice> delete bar
```

``` unison :hide
foo = 19
qux = 20
```

``` ucm :hide
scratch/alice> update

scratch/alice> switch /main

scratch/main> branch bob
```

``` unison :hide
baz = 21
bar = foo + foo + foo
```

``` ucm :hide
scratch/bob> update
```

``` ucm
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

You may also provide reflog entries:

``` ucm
scratch/bob> reflog

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

       Branch        When   Hash          Description
  1.   scratch/bob   now    #diiate0po6   update
  2.   scratch/bob   now    #umhbc4jt4j   Branch created from scratch/main

scratch/bob> branch.diff 2 1

  Changes on
  #diiate0po6jk4gui8v40arjcm4v128qk5khctdjbthbd991viqmmsup52jhtvp93uavnfkdpc6ena6ubhmb4na6jefcqkvfjp9kimbg:

  ~ bar : Nat
  ~ baz : Nat

  ~ (modified)
```

``` ucm :hide
scratch/main> project.delete scratch
```

If two branches are identical, we just get a textual message (no diff).

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` ucm :hide
scratch/main> branch alice
```

``` ucm
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

``` unison :hide
foo = 17
bar = 18
```

``` ucm :hide
scratch/main> update

scratch/main> move.term foo lib.foo.foo

scratch/main> move.term bar lib.bar.bar

scratch/main> branch alice

scratch/alice> delete.force lib.foo.foo
```

``` unison :hide
foo = 18
baz = 19
```

``` ucm :hide
scratch/alice> update

scratch/alice> move.term foo lib.foo.foo

scratch/alice> move.term baz lib.baz.baz

scratch/alice> delete.namespace lib.bar
```

``` ucm
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

``` unison :hide
main = "main"
```

``` ucm :hide
scratch/main> update

scratch/main> branch.create-empty topic
```

``` unison :hide
topic = "topic"
```

``` ucm :hide
scratch/topic> update
```

``` ucm
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

``` unison :hide
thing = 17
foo = thing + thing
```

``` ucm :hide
scratch/main> update

scratch/main> move thing lib.dep_1_0_0.thing

scratch/main> branch topic
```

``` unison :hide
thing = 18
```

``` ucm :hide
scratch/topic> update

scratch/topic> move thing lib.dep_2_0_0.thing

scratch/topic> upgrade dep_1_0_0 dep_2_0_0
```

``` ucm
scratch/topic> diff.branch /main /topic

  Changes on /topic:

  + lib.dep_2_0_0
  - lib.dep_1_0_0

  + (added), - (deleted)
```

``` ucm :hide
scratch/main> project.delete scratch
```
