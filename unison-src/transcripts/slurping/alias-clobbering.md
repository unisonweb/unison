# Aliasing takes priority over provided definition

```ucm:hide
.> builtins.merge
```

```unison
x = 1
```

```ucm
.> add
```

```unison
-- Overwrite the existing alias
x = 2
-- But add a _new_ definition that's an alias of the _old_ x
y = 1
```

We see that `y` ends up as an alias for the new value of `x`, i.e. `y = 2`,
even though we explicitly said `y = 1`!

Note how `update` isn't idempotent, it ping-pongs back and forth between aliases:

```ucm
.> update
.> view x
.> view y
.> update
.> update
.> update
```
