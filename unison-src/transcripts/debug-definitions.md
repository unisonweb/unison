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
.> debug.term y
.> debug.term.verbose y
.> debug.term Some
.> debug.term.verbose Some
.> debug.term ask
.> debug.term.verbose ask
.> debug.type Optional
.> debug.type Ask
```
