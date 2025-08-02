---
autoupdate: false
type: tutorial
---

This transcript explicitly sets the type to `tutorial`, so

this code block should hide its output

``` unison
foo = ()
```

but this should succeed, because the tutorial’s `autoupdate` behavior has been overridden by the explicit `autoupdate` setting.

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
