This transcript tests that UCM can always access the definition of
`IsPropagated`/`isPropagated`, which is used internally.

`y` depends on `x`,
```unison
x = 3
y = x + 1
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat
    y : Nat

```
so the `update` of `x` causes a propagated update of `y`, and UCM links the
`isPropagated` metadata to such resulting terms:

```unison
x = 4
```

```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    x : Nat

.> links y

  1. #kvjtpqi06m : #pi9o52ongq
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.> view 1

  #kvjtpqi06m : #pi9o52ongq
  #kvjtpqi06m = #pi9o52ongq#0

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
