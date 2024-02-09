# Replace with terms and types

Let's set up some definitions to start:

```ucm:hide
.lib> builtins.merge
```

```unison
x = 1
y = 2

structural type X = One Nat
structural type Y = Two Nat Nat
```

```ucm
.> add
```

Test that replace works with terms
```ucm
.> replace x y
.> view x
```

Test that replace works with types
```ucm
.> replace X Y
.> find
.> view.patch patch
.> view X
```

Try with a type/term mismatch
```ucm:error
.> replace X x
```
```ucm:error
.> replace y Y
```

Try with missing references
```ucm:error
.> replace X NOPE
```
```ucm:error
.> replace y nope
```
```ucm:error
.> replace nope X
```
```ucm:error
.> replace nope y
```
```ucm:error
.> replace nope nope
```
