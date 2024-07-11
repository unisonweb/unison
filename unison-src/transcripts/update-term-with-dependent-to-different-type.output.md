``` ucm
scratch/main> builtins.merge

  Done.

```
``` unison
foo : Nat
foo = 5

bar : Nat
bar = foo + 10
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

```
``` unison
foo : Int
foo = +5
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Int

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
bar : Nat
bar =
  use Nat +
  foo + 10

foo : Int
foo = +5
```

