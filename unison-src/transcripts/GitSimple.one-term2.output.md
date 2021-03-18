```unison
c = 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      c : ##Nat

```
```ucm
.> debug.file

  c#msp7bv40rv

  ☝️  The namespace .myLib is empty.

.myLib> add

  ⍟ I've added these definitions:
  
    c : ##Nat

.myLib> push /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-one-term2-6e1967b10dc504d9/repo.git

  Done.

```

-------
```ucm
  ☝️  The namespace .yourLib is empty.

.yourLib> pull /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-one-term2-6e1967b10dc504d9/repo.git

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. c : ##Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```unison
> c
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > c
          ⧩
          3

```
