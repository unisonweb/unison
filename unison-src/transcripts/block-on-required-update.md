# Block on required update

Should block an `add` if it requires an update on an in-file dependency.

```ucm:hide
scratch/main> builtins.merge
```

```unison
x = 1
```

```ucm
scratch/main> add
```

Update `x`, and add a new `y` which depends on the update

```unison
x = 10
y = x + 1
```

Try to add only the new `y`. This should fail because it requires an update to `x`, but we only ran an 'add'.

```ucm:error
scratch/main> add y
```
