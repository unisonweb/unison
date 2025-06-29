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

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      structural type Z
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type Y
        (The old definition is also named builtin.Unit.)
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
