Updating part of a cycle, but not all, should generally do the right thing.

```ucm
.> builtins.merge
```

```unison
ping : 'Nat
ping _ = !pong + 1

pong : 'Nat
pong _ = !ping + 2
```

```ucm
.> add
```

Here we get ready to update only `ping`, which (at the time of the initial typecheck) refers to `pong`, which itself
refers to the *old* `ping`.

```unison
ping : 'Nat
ping _ = !pong + 3
```

Yet, when we actually perform the `update`, both `ping` and `pong` are updated (where the updated `ping` refers to the
updated `pong`, and vice versa).

```ucm
.> update
.> view ping pong
```

"Topology changes" currently only work correctly if the changes are localized to a component.

For example, updating from `ping <-> pong` to `ping <- pong`, as in the following example.

```unison
ping : 'Nat
ping _ = 808
```

```ucm
.> update
.> view ping pong
```

Not yet working: updating from `ping <- pong` to `ping <-> pong`.

```unison
ping : 'Nat
ping _ = !pong + 1
```

```ucm
.> update
.> view ping pong
.> undo
```

If we perform a type-changing update to a member of a cycle, the update will go through, as normal.

```unison
ping : Nat
ping = 808
```

```ucm
.> update
.> view ping pong
.> undo
```
