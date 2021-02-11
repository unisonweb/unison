This transcript tests that UCM can always access the definition of 
`IsPropagated`, which is used internally.

y depends on x
```unison
x = 3
y = x + 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat
      y : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat
    y : Nat

```
```unison
x = 4
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat

```
The `update` of `x` causes a propagated update of `y`, and UCM links the 
`isPropagated` metadata to such resulting terms:

```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    x : Nat

.> links y

  1. #uqdd5t2fgn : #ffb7g9cull
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.> view 1

  #uqdd5t2fgn : #ffb7g9cull
  #uqdd5t2fgn = #ffb7g9cull#0

```
Well, it's hard to tell from those hashes, but those are right.  We can confirm
by running `builtins.merge` to have UCM add names for them.

```ucm
.> builtins.merge

  Done.

.> links y

  1. builtin.metadata.isPropagated : IsPropagated
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.> view 1

  builtin.metadata.isPropagated : IsPropagated
  builtin.metadata.isPropagated = IsPropagated

```
