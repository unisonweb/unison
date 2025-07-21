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

  + hangExample : Boolean

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
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
