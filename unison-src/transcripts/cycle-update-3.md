Update a member of a cycle with a type-changing update, thus severing the cycle.

```ucm:hide
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

```unison
ping : Nat
ping = 3
```

```ucm
.> update
.> view ping pong
```
