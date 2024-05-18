# projects merge

```ucm
.> builtins.merge
```

```unison
zonk = 0
```

```ucm
.foo> add
.> project.create-empty foo
.> merge.old foo foo/main
```

```unison
bonk = 2
```

```ucm
foo/main> add
```

```ucm
.> project.create-empty bar
bar/main> merge.old foo/main
bar/main> branch /topic
```

```unison
xonk = 1
```

```ucm
bar/main> add
bar/topic> merge.old /main
.bar> merge.old foo/main
```
