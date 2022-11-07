```unison
a.b.one = 1
a.two = 2

a.x.three = 3
a.x.four = 4

structural type a.x.Foo = Foo | Bar
structural type a.b.Baz = Boo
```

```ucm
.> add
.> delete.term a.b.one
.> alias.term a.two a.newtwo
.> move.namespace a.x a.y
.> history
.> debug.name-diff 4 1
```
