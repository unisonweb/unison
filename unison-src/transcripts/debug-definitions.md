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
.> debug.term.abt Nat.+
.> debug.term.abt y
.> debug.term.abt Some
.> debug.term.abt ask
.> debug.type.abt Nat
.> debug.type.abt Optional
.> debug.type.abt Ask
```
