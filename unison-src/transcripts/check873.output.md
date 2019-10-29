See [this ticket](https://github.com/unisonweb/unison/issues/873); the point being, this shouldn't crash the runtime. :)

```unison
(-) = builtin.Nat.sub
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      - : builtin.Nat -> builtin.Nat -> builtin.Int
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    - : builtin.Nat -> builtin.Nat -> builtin.Int

```
```unison
baz x = x - 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      baz : builtin.Nat -> builtin.Int
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
