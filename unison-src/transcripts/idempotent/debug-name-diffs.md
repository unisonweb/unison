``` unison
a.b.one = 1
a.two = 2

a.x.three = 3
a.x.four = 4

structural type a.x.Foo = Foo | Bar
structural type a.b.Baz = Boo
```

``` ucm
scratch/main> add
scratch/main> delete.term.verbose a.b.one
scratch/main> alias.term a.two a.newtwo
scratch/main> move.namespace a.x a.y
scratch/main> history
scratch/main> debug.name-diff 4 1
```
