# Adds and updates

Let's set up some definitions to start:

```ucm:hide
.> builtins.merge
```

```unison
x = 1
y = 2

structural type X = One Nat
structural type Y = Two Nat Nat
```

Expected: `x` and `y`, `X`, and `Y` exist as above. UCM tells you this.

```ucm
.> add
```

Let's add an alias for `1` and `One`:

```unison
z = 1

structural type Z = One Nat
```

Expected: `z` is now `1`. UCM tells you that this definition is also called `x`.
Also, `Z` is an alias for `X`.

```ucm
.> add
```

Let's update something that has an alias (to a value that doesn't have a name already):

```unison
x = 3
structural type X = Three Nat Nat Nat
```

Expected: `x` is now `3` and `X` has constructor `Three`. UCM tells you the old definitions were also called `z` and `Z` and these names have also been updated.

```ucm
.> update
```

Update it to something that already exists with a different name:

```unison
x = 2
structural type X = Two Nat Nat
```

Expected: `x` is now `2` and `X` is `Two`. UCM says the old definition was also named `z/Z`, and was also updated. And it says the new definition is also named `y/Y`.

```ucm
.> update
```

