# Aliasing takes priority over provided definition

```unison
x = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat

```
```unison
-- Overwrite the existing alias
x = 2
-- But add a _new_ definition that's an alias of the _old_ x
y = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      y : Nat
        (also named x)
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat

```
We see that `y` ends up equal to the new value of `x`, i.e. `y = 2`,
even though we explicitly said `y = 1`!

```ucm
.> update

  ⍟ I've added these definitions:
  
    y : Nat
      (also named x)
  
  ⍟ I've updated these names to your new definition:
  
    x : Nat

.> view x

  x : Nat
  x = 2

.> view y

  x : Nat
  x = 2

```
