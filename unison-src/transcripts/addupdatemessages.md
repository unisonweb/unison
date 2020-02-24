# Adds and updates

Let's set up some definitions to start:

```unison
x = 1
y = 2
```

Expected: `x` and `y` exist as above. UCM tells you this.

```ucm
.scratch> add
```

Let's add an alias for `1`:

```unison
z = 1
```

Expected: `z` is now `1`. UCM tells you that this definition is also called `x`.

```ucm
.scratch> add
```

Let's update something that has an alias (to a value that doesn't have a name already):

```unison
x = 3
```

Expected: `x` is now `3`. UCM tells you the old definition was also called `z` and this name has also been updated.

```ucm
.scratch> update
```

Update it to something that already exists with a different name:

```unison
x = 2
```

Expected: `x` is now `2`. UCM says the old definition was also named `z`, and was also updated. And it says the new definition is also named `y`. 

```ucm
.scratch> update
```

