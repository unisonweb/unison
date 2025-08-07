Regression test for https://github.com/unisonweb/unison/pull/2819

``` ucm :hide
> builtins.merge
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

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view hangExample

  hangExample : Boolean
  hangExample =
    "a long piece of text to hang the line" == ""
      && "a long piece of text to hang the line" == ""
```
