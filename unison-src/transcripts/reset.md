```ucm:hide
scratch/main> builtins.merge
```

```unison
def = "first value"
```

```ucm:hide
scratch/main> update
```

```unison:hide
def = "second value"
```

Can reset to a value from history by number.

```ucm
scratch/main> update
scratch/main> history
scratch/main> reset 2
scratch/main> view def
scratch/main> history
```

Can reset to a value from reflog by number.

```ucm
scratch/main> reflog
-- Reset the current branch to the first history element
scratch/main> reset 2
scratch/main> view def
scratch/main> history
```

# reset branch

```ucm
foo/main> history
```

```unison:hide
a = 5
```

```ucm
foo/main> update
foo/empty> reset /main:
foo/empty> view a
foo/empty> history
```

## second argument is always interpreted as a branch
```unison:hide
main.a = 3
```

```ucm
foo/main> update
foo/main> history
foo/main> reset 2 main
```
