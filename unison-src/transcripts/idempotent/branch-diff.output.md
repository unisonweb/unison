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

You may also provide the hash of a branch:

``` ucm
scratch/bob> reflog
  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.
  Tip: Use `diff.namespace 1 7` to compare between points in
       history.
       Branch        Hash          Description
  1.   scratch/bob   #idu8gij628   update
  2.   scratch/bob   #glh59bml46   Branch created from scratch/main
scratch/bob> branch.diff #glh59bml46 1
  Changes on
  #idu8gij628bch8jnm3aqdmnc2mr5ureie9bhci9ob7pv27cmequ4534lqklnhdu9jitkc1uhh6ku1n0n4fmcrlu1kktvi2lm6ovduqo:
  ~ bar : Nat
  ~ baz : Nat
  ~ (modified)
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
😶

I don't know of a namespace with that hash.
```
