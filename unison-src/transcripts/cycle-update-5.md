Not yet working: properly updating nameless implicit terms.

```ucm
.> builtins.merge
```

```unison
inner.ping : 'Nat
inner.ping _ = !pong + 1

pong : 'Nat
pong _ = !inner.ping + 2
```

N.B. `find.verbose` on `pong` just to put its hash into the transcript output, so it's easier to copy if we change
hashing.

```ucm
.> add
```

Here we queue up an update by saving in a namespace where `inner.ping` and `pong` both have names, but then apply the
update in a namespace where only `ping` has a name.

```unison
inner.ping : 'Nat
inner.ping _ = !pong + 3
```

```ucm
.inner> update
.> view inner.ping
```

The bug here is that `inner.ping` still refers to `pong` by name. But if we properly identified the nameless (in the
context that the update was applied) `pong` as an implicit term to include in the new `ping`'s cycle, then `ping` would
be left referring to a nameless thing (namely, `pong`, but updated to refer to the new `ping`).
