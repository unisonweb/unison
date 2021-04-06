```ucm
  ☝️  The namespace .myLib is empty.

.myLib> alias.term ##Nat.+ +

  Done.

```
```unison
improveNat x = x + 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      improveNat : ##Nat -> ##Nat

```
```ucm
.myLib> add

  ⍟ I've added these definitions:
  
    improveNat : ##Nat -> ##Nat

.myLib> ls

  1. +          (##Nat -> ##Nat -> ##Nat)
  2. improveNat (##Nat -> ##Nat)

.myLib> move.namespace .myLib .workaround1552.myLib.v1

  Done.

.workaround1552.myLib> ls

  1. v1/ (2 definitions)

.workaround1552.myLib> fork v1 v2

  Done.

```
```unison
improveNat x = x + 100
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      improveNat : ##Nat -> ##Nat

```
```ucm
.workaround1552.myLib.v2> update

  ⍟ I've updated these names to your new definition:
  
    improveNat : ##Nat -> ##Nat

.workaround1552.myLib> push /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-patching-7fa752c409f898ef/repo.git

  Done.

```

-------
```ucm
  ☝️  The namespace .myApp is empty.

.myApp> pull /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-patching-7fa752c409f898ef/repo.git:.v1 external.yourLib

  Here's what's changed in external.yourLib after the merge:
  
  Added definitions:
  
    1. +          : ##Nat -> ##Nat -> ##Nat
    2. improveNat : ##Nat -> ##Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.myApp> alias.term ##Nat.* *

  Done.

```
`
```unison
> greatApp = improveNat 5 * improveNat 6
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > greatApp = improveNat 5 * improveNat 6
          ⧩
          72

```
```ucm
.myApp> add

  

.myApp> pull /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-patching-7fa752c409f898ef/repo.git:.v2 external.yourLib

  Here's what's changed in external.yourLib after the merge:
  
  Updates:
  
    1. improveNat : ##Nat -> ##Nat
       ↓
    2. improveNat : ##Nat -> ##Nat
  
  Added definitions:
  
    3. patch patch (added 1 updates)
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```unison
> greatApp = improveNat 5 * improveNat 6
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > greatApp = improveNat 5 * improveNat 6
          ⧩
          11130

```
```ucm
.myApp> patch external.yourLib.patch

  😶
  
  This had no effect. Perhaps the patch has already been applied
  or it doesn't intersect with the definitions in
  the current namespace.

```
```unison
> greatApp = improveNat 5 * improveNat 6
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > greatApp = improveNat 5 * improveNat 6
          ⧩
          11130

```
