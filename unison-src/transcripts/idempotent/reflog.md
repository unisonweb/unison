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
  1.   scratch/main   #actc8cchvq   update
  2.   scratch/main   #i2p77k1ci0   update
  3.   scratch/main   #btddbo4t1j   builtins.merge scratch/main:lib.builtins
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
  1.   scratch/other   #lko9jdvg1c   alias.term y scratch/other:z
  2.   scratch/other   #actc8cchvq   Branch created from scratch/main
  3.   scratch/main    #actc8cchvq   update
  4.   scratch/main    #i2p77k1ci0   update
  5.   scratch/main    #btddbo4t1j   builtins.merge scratch/main:lib.builtins
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
  1.   newproject/main   #6bbdh6rbo9   alias.type lib.builtins.Nat .MyNat
  2.   newproject/main   #btddbo4t1j   builtins.merge newproject/main:lib.builtins
  3.   newproject/main   #sg60bvjo91   Branch Created
  4.   scratch/other     #lko9jdvg1c   alias.term y scratch/other:z
  5.   scratch/other     #actc8cchvq   Branch created from scratch/main
  6.   scratch/main      #actc8cchvq   update
  7.   scratch/main      #i2p77k1ci0   update
  8.   scratch/main      #btddbo4t1j   builtins.merge scratch/main:lib.builtins
  9.   scratch/main      #sg60bvjo91   Project Created
```
