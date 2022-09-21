Update a member of a cycle, but retain the cycle.

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

```unison
ping : 'Nat
ping _ = !pong + 3
```

```ucm
.> update
.> view ping pong
```
