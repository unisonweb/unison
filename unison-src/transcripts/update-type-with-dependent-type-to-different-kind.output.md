``` unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Baz
      type Foo

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Baz
    type Foo

```
``` unison
unique type Foo a = Bar Nat a
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo a

```
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
type Baz = Qux Foo

type Foo a = Bar Nat a
```

