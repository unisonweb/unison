Updating part of a cycle, but not all, should generally do the right thing.

```ucm
.> builtins.merge

  Done.

```
```unison
ping : 'Nat
ping _ = !pong + 1

pong : 'Nat
pong _ = !ping + 2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ping : 'Nat
      pong : 'Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    ping : 'Nat
    pong : 'Nat

```
Here we get ready to update only `ping`, which (at the time of the initial typecheck) refers to `pong`, which itself
refers to the *old* `ping`.

```unison
ping : 'Nat
ping _ = !pong + 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : 'Nat

```
Yet, when we actually perform the `update`, both `ping` and `pong` are updated (where the updated `ping` refers to the
updated `pong`, and vice versa).

```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    ping : 'Nat
    pong : 'Nat

.> view ping pong

  ping : 'Nat
  ping _ =
    use Nat +
    !pong + 3
  
  pong : 'Nat
  pong _ =
    use Nat +
    !ping + 2

```
"Topology changes" currently only work correctly if the changes are localized to a component.

For example, updating from `ping <-> pong` to `ping <- pong`, as in the following example.

```unison
ping : 'Nat
ping _ = 808
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : 'Nat

```
```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    ping : 'Nat
    pong : 'Nat

.> view ping pong

  ping : 'Nat
  ping _ = 808
  
  pong : 'Nat
  pong _ =
    use Nat +
    !ping + 2

```
Not yet working: updating from `ping <- pong` to `ping <-> pong`.

```unison
ping : 'Nat
ping _ = !pong + 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : 'Nat

```
```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    ping : 'Nat

.> view ping pong

  ping : 'Nat
  ping _ =
    use Nat +
    !pong + 1
  
  pong : 'Nat
  pong _ =
    use Nat +
    !#vp4dk180mu + 2

.> undo

  Here are the changes I undid
  
  Updates:
  
    There were 2 auto-propagated updates.

```
If we perform a type-changing update to a member of a cycle, the update will go through, as normal.

```unison
ping : Nat
ping = 808
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : Nat

```
```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    ping : Nat

.> view ping pong

  ping : Nat
  ping = 808
  
  pong : 'Nat
  pong _ =
    use Nat +
    !#ehrgua9o2q + 2

.> undo

  Here are the changes I undid
  
  Updates:
  
    1. ping : 'Nat
       ↓
    2. ping : Nat
  
    3. patch patch (added 4 updates, deleted 4)

```
