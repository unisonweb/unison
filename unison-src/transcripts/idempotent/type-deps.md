# Ensure type dependencies are properly considered in slurping

https://github.com/unisonweb/unison/pull/2821

``` ucm :hide
scratch/main> builtins.merge
```

Define a type.

``` unison :hide
structural type Y = Y
```

``` ucm :hide
scratch/main> add
```

Now, we update `Y`, and add a new type `Z` which depends on it.

``` unison
structural type Z = Z Y
structural type Y = Y Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Z
  ~ structural type Y

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

-- This shouldn't exist, because it should've been blocked.

scratch/main> view Z

  structural type Z = Z Y
```
