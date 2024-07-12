```unison
foo = 5
foo.bar = 1
```

```ucm
p0/main> add
```

```unison
bonk = 5
donk.bonk = 1
```

```ucm
p1/main> add
p1/main> fork p0/main: zzz
p1/main> find zzz
p1/main> fork p0/main:foo yyy
p1/main> find yyy
p0/main> fork p1/main: p0/main:p1
p0/main> ls p1
p0/main> ls p1.zzz
p0/main> ls p1.yyy
```
