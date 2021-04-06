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

.> add

  ⍟ I've added these definitions:
  
    c : ##Nat

.> push /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-one-term-ca8a9c15aa27005c/repo.git

  Done.

```

-------
```ucm
.> pull /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-one-term-ca8a9c15aa27005c/repo.git

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. c : ##Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> alias.term ##Nat.+ +

  Done.

```
```unison
> #msp7bv40rv + 1
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > #msp7bv40rv + 1
          ⧩
          4

```
