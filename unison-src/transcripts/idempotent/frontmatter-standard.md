---
type: standard
---

This transcript has the `standard` type, so it should behave in the standard manner.

I.e., this code block should show its output

``` unison
foo = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      foo : ()
```

And this should add the definition:

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
