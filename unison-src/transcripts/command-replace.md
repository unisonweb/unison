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
.scratch> replace x y
.scratch> view x
```

Test that replace works with types
```ucm
.scratch> replace X Y
.scratch> view X
```

Try with a type/term mismatch
```ucm:error
.scratch> replace X x
```
```ucm:error
.scratch> replace y Y 
```

Try with missing references
```ucm:error
.scratch> replace X NOPE
```
```ucm:error
.scratch> replace y nope
```
```ucm:error
.scratch> replace nope X
```
```ucm:error
.scratch> replace nope y
```
```ucm:error
.scratch> replace nope nope
```