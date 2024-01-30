```ucm:hide
.> builtins.merge
```

```unison:hide
x = 30

y : Nat
y = 
  z = x + 2
  z + 10

structural type Optional a = Some a | None

ability Ask a where
  ask : a
```

```ucm
.> add
.> debug.term Nat.+
.> debug.term y
.> debug.term Some
.> debug.term ask
.> debug.type Nat
.> debug.type Optional
.> debug.type Ask
```
