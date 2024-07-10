``` unison
a.x.x.x.x = 100
b.x.x.x.x = 100
foo = 25
c.y.y.y.y = foo + 10
d.y.y.y.y = foo + 10
bar = a.x.x.x.x + c.y.y.y.y
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a.x.x.x.x : Nat
      b.x.x.x.x : Nat
      bar       : Nat
      c.y.y.y.y : Nat
      d.y.y.y.y : Nat
      foo       : Nat

```
``` ucm
myproject/main> add

  ⍟ I've added these definitions:
  
    a.x.x.x.x : Nat
    b.x.x.x.x : Nat
    bar       : Nat
    c.y.y.y.y : Nat
    d.y.y.y.y : Nat
    foo       : Nat

```
``` unison
foo = +30
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
myproject/main> update

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
  x + c.y.y.y.y

c.y.y.y.y : Nat
c.y.y.y.y =
  use Nat +
  foo + 10

d.y.y.y.y : Nat
d.y.y.y.y =
  use Nat +
  foo + 10

foo = +30
```

