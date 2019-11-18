```unison
x = 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : builtin.Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
  ☝️  The namespace .foo is empty.

.foo> add

  ⍟ I've added these definitions:
  
    x : .builtin.Nat

.foo> push /tmp/test1.git

  Done.

```
Now we pull what we pushed
```ucm
  ☝️  The namespace .foo2 is empty.

.foo2> pull /tmp/test1.git

  🆕
  
  Here's what's changed in the current namespace after the pull:
  
  + Adds / updates:
  
    x
  
  Tip: You can always `undo` if this wasn't what you wanted.

.foo2> ls

  1. x (.builtin.Nat)

```
