```ucm
.> alias.term ##Nat.+ +

  Done.

```
```unison
type Foo = Foo | Bar
a = 3
b = a + 1
```

```ucm
  ☝️  The namespace .foo.bar is empty.

.foo.bar> add

  ⍟ I've added these definitions:
  
    type Foo
    a : ##Nat
    b : ##Nat

```
```unison
a = 4
```

```ucm
.foo.bar> update

  ⍟ I've updated these names to your new definition:
  
    a : ##Nat

.> find

  1. + : ##Nat -> ##Nat -> ##Nat
  2. type foo.bar.Foo
  3. foo.bar.Foo.Bar : Foo
  4. foo.bar.Foo.Foo : Foo
  5. foo.bar.a : ##Nat
  6. foo.bar.b : ##Nat
  

```
```unison
> b
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > b
          ⧩
          5

```
