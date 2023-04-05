# projects merge

```ucm
.> builtins.merge
```

```unison
zonk = 0
```

```ucm
.foo> add
.> project.create foo
.> merge foo foo/main
```

```unison
bonk = 2
```

```ucm
foo/main> add
```

```ucm
.> project.create bar
bar/main> merge foo/main
bar/main> switch /topic
```

```unison
xonk = 1
```

```ucm
bar/main> add
bar/topic> merge /main
.bar> merge foo/main
```
