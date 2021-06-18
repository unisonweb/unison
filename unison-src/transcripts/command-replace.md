# Replace with terms and types

Let's set up some definitions to start:

```ucm:hide
.> builtins.merge
```

```unison
x = 1
y = 2

type X = One Nat
type Y = Two Nat Nat
```

```ucm
.scratch> add
```

Test that replace works with terms
```ucm
.scratch> replace.any x y
.scratch> view x
```

Test that replace works with types
```ucm
.scratch> replace.any X Y
.scratch> view X
```

Try with a type/term mismatch
```ucm:error
.scratch> replace.any X x
```
```ucm:error
.scratch> replace.any y Y 
```

Try with missing references
```ucm:error
.scratch> replace.any X NOPE
```
```ucm:error
.scratch> replace.any y nope
```
```ucm:error
.scratch> replace.any nope X
```
```ucm:error
.scratch> replace.any nope y
```
```ucm:error
.scratch> replace.any nope nope
```