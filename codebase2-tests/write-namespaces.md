```ucm
.> alias.term ##Nat.+ +
```

```unison:hide
type Foo = Foo | Bar
a = 3
b = a + 1
```

```ucm
.foo.bar> add
```

```unison:hide
a = 4
```

```ucm
.foo.bar> update
.> find
```

```unison
> b
```