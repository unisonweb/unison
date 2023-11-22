```unison
unique type Foo = { bar : Nat }
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo

```
This shouldn't be an error.

```ucm
.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  I couldn't complete the update because I couldn't find 0
  constructor(s) for Foo where I expected to. I found: []
  
  You can use `view Foo` and
  `alias.term <hash> Foo.<ConstructorName>` to give names to
  each constructor, and then try again.

.> view Foo

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    Foo

```
