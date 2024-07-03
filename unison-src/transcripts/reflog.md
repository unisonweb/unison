```ucm:hide
scratch/main> builtins.merge lib.builtins
```

First we make two changes to the codebase, so that there's more than one line
for the `reflog` command to display:

```unison
x = 1
```
```ucm
scratch/main> add
```
```unison
y = 2
```
```ucm
scratch/main> add
scratch/main> view y
```
```ucm
scratch/main> reflog
```

If we `reset-root` to its previous value, `y` disappears.
```ucm
scratch/main> reset-root 2
```
```ucm:error
scratch/main> view y
```
