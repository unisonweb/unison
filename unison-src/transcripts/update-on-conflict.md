# Update on conflict

Updating conflicted definitions works fine.

```ucm:hide
.> builtins.merge
.merged> builtins.merge
```

```unison
x = 1
temp = 2
```

```ucm
.> add
.> debug.alias.term.force temp x
.> delete.term temp
```

```unison
x = 3
```

```ucm
.> update
.> view x
```
