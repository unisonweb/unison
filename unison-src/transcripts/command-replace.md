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
.scratch> replace.term x y
.scratch> view x
```

Test that replace works with types
```ucm
.scratch> replace.term X Y
.scratch> view X
```

Try with a type/term mismatch
```ucm:error
.scratch> replace.term X x
```
```ucm:error
.scratch> replace.term y Y 
```

Try with missing references
```ucm:error
.scratch> replace.term X NOPE
```
```ucm:error
.scratch> replace.term y nope
```
```ucm:error
.scratch> replace.term nope X
```
```ucm:error
.scratch> replace.term nope y
```
```ucm:error
.scratch> replace.term nope nope
```