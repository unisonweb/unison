Regression test for https://github.com/unisonweb/unison/pull/2819

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
hangExample : Boolean
hangExample =
  ("a long piece of text to hang the line" == "")
    && ("a long piece of text to hang the line" == "")
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      hangExample : Boolean
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view hangExample

  hangExample : Boolean
  hangExample =
    "a long piece of text to hang the line" == ""
      && "a long piece of text to hang the line" == ""
```
