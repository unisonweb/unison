``` ucm :hide
myproject/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 25
lib.new.foo = +30
a.x.x.x.x = 100
b.x.x.x.x = 100
c.y.y.y.y = lib.old.foo + 10
d.y.y.y.y = lib.old.foo + 10
bar = a.x.x.x.x + c.y.y.y.y
```

``` ucm
myproject/main> add
```

``` ucm :error
myproject/main> upgrade old new
```
