```ucm:hide
scratch/main> builtins.merge
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
scratch/main> add
scratch/main> debug.term.abt Nat.+
scratch/main> debug.term.abt y
scratch/main> debug.term.abt Some
scratch/main> debug.term.abt ask
scratch/main> debug.type.abt Nat
scratch/main> debug.type.abt Optional
scratch/main> debug.type.abt Ask
```
