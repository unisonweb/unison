This transcript has empty frontmatter, so it should behave in the standard manner (and the empty frontmatter should be omitted from the output).

I.e., this code block should show its output

``` unison
foo = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : ()

  Run `update` to apply these changes to your codebase.
```

And this should add the definition:

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
