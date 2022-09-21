Update a member of a cycle with a type-changing update, thus severing the cycle.

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
```unison
ping : Nat
ping = 3
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
  ping = 3
  
  pong : 'Nat
  pong _ =
    use Nat +
    !#4t465jk908.1 + 2

```
