```ucm:hide
.> builtins.merge
```

First we make two changes to the codebase, so that there's more than one line
for the `reflog` command to display:

```unison
x = 1
```
```ucm
.> add
```
```unison
y = 2
```
```ucm
.> add
.> view y
```
```ucm
.> reflog
```

If we `reset-root` to its previous value, `y` disappears.
```ucm
.> reset-root 2
```
```ucm:error
.> view y
```
