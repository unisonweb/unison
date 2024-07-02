# Update on conflict

Updating conflicted definitions works fine.

```ucm:hide
scratch/main> builtins.merge lib.builtins
```

```unison
x = 1
temp = 2
```

```ucm
scratch/main> add
scratch/main> debug.alias.term.force temp x
scratch/main> delete.term temp
```

```unison
x = 3
```

```ucm
scratch/main> update
scratch/main> view x
```
