`update` properly discovers and establishes new cycles.

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
ping _ = !clang + 1

clang : 'Nat
clang _ = !pong + 3
```

```ucm
.> update ping
.> view ping pong clang
```
