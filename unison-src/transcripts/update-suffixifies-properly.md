```ucm:hide
.> project.create-empty myproject
myproject/main> builtins.merge
myproject/main> move.namespace builtin lib.builtin
```

```unison
a.x.x.x.x = 100
b.x.x.x.x = 100
foo = 25
c.y.y.y.y = foo + 10
d.y.y.y.y = foo + 10
bar = a.x.x.x.x + c.y.y.y.y
```

```ucm
myproject/main> add
```

```unison
foo = +30
```

```ucm:error
myproject/main> update
```
