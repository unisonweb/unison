``` unison
unique type Foo = Bar Nat

structural type A.B = OneAlias Foo
structural type A = B.TheOtherAlias Foo
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type A
      structural type A.B
      type Foo

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    structural type A
    structural type A.B
    type Foo

```
``` unison
unique type Foo = Bar Nat Nat
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo

```
Bug: we want this update to be rejected earlier, because it violates the "decl coherency" precondition that there's
only one name for each constructor. We instead get too far in the update process, and are delivered a bogus scratch.u
file to stare at.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.

```
``` unison:added-by-ucm scratch.u
structural type A = B.OneAlias Foo

structural type A.B = OneAlias Foo

type Foo = Bar Nat Nat
```

