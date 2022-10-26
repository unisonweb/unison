Not yet working: properly updating nameless implicit terms.

```unison
inner.ping : 'Nat
inner.ping _ = !pong + 1

pong : 'Nat
pong _ = !inner.ping + 2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      inner.ping : 'Nat
      pong       : 'Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    inner.ping : 'Nat
    pong       : 'Nat

```
Here we queue up an update by saving in a namespace where `inner.ping` and `pong` both have names, but then apply the
update in a namespace where only `ping` has a name.

```unison
inner.ping : 'Nat
inner.ping _ = !pong + 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      inner.ping : 'Nat

```
```ucm
.inner> update

  ⍟ I've added these definitions:
  
    inner.ping : 'Nat

.> view inner.ping

  inner.ping : 'Nat
  inner.ping _ =
    use Nat +
    !pong + 1

```
The bug here is that `inner.ping` still refers to `pong` by name. But if we properly identified the nameless (in the
context that the update was applied) `pong` as an implicit term to include in the new `ping`'s cycle, then `ping` would
be left referring to a nameless thing (namely, `pong`, but updated to refer to the new `ping).
