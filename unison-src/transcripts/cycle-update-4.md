Not yet working: using update to establish a cycle.

```ucm:hide
.> builtins.merge
```

```unison
ping : 'Nat
ping _ = 1

pong : 'Nat
pong _ = !ping + 2
```

```ucm
.> add
```

```unison
ping : 'Nat
ping _ = !pong + 1
```

```ucm
.> update
.> view ping pong
```

The bug: `pong` should refer to the new `ping` by name, not the (now nameless) old `ping`.
