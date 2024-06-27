Update a member of a cycle, but retain the cycle.

```ucm:hide
scratch/main> builtins.merge
```

```unison
ping : 'Nat
ping _ = !pong + 1

pong : 'Nat
pong _ = !ping + 2
```

```ucm
scratch/main> add
```

```unison
ping : 'Nat
ping _ = !pong + 3
```

```ucm
scratch/main> update
scratch/main> view ping pong
```
