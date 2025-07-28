``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

First we make some changes to the codebase so there's data in the reflog.

``` unison
x = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + x : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
y = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + y : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> branch /other

  Done. I've created the other branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /other`.

scratch/other> alias.term y z

  Done.

newproject/main> builtins.merge lib.builtins

  Done.

newproject/main> alias.type lib.builtins.Nat MyNat

  Done.
```

Should see reflog entries from the current branch

``` ucm
scratch/main> reflog

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

       Branch         Hash          Description
  1.   scratch/main   #jjvgtsthgs   update
  2.   scratch/main   #d8edma69hk   update
  3.   scratch/main   #qg4gmfn7js   builtins.merge scratch/main:lib.builtins
  4.   scratch/main   #sg60bvjo91   Project Created
```

Should see reflog entries from the current project

``` ucm
scratch/main> project.reflog

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

       Branch          Hash          Description
  1.   scratch/other   #9kakel8qsf   alias.term y scratch/other:z
  2.   scratch/other   #jjvgtsthgs   Branch created from scratch/main
  3.   scratch/main    #jjvgtsthgs   update
  4.   scratch/main    #d8edma69hk   update
  5.   scratch/main    #qg4gmfn7js   builtins.merge scratch/main:lib.builtins
  6.   scratch/main    #sg60bvjo91   Project Created
```

Should see reflog entries from all projects

``` ucm
scratch/main> reflog.global

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

       Branch            Hash          Description
  1.   newproject/main   #ehflitn3vj   alias.term lib.builtins.Nat newproject/main:MyNat
  2.   newproject/main   #qg4gmfn7js   builtins.merge newproject/main:lib.builtins
  3.   newproject/main   #sg60bvjo91   Branch Created
  4.   scratch/other     #9kakel8qsf   alias.term y scratch/other:z
  5.   scratch/other     #jjvgtsthgs   Branch created from scratch/main
  6.   scratch/main      #jjvgtsthgs   update
  7.   scratch/main      #d8edma69hk   update
  8.   scratch/main      #qg4gmfn7js   builtins.merge scratch/main:lib.builtins
  9.   scratch/main      #sg60bvjo91   Project Created
```
