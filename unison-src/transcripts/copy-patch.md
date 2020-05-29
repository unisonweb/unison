# Test that copying a patch works as expected

```unison
x = 1
```

```ucm
.> add
```

Change the definition of `x` so something goes in our patch:

```unison
x = 2
```

```ucm
.> update foo.patch
```

Copy the patch and make sure it's still there.

```ucm
.> copy.patch foo.patch bar.patch
.> view.patch foo.patch
.> view.patch bar.patch
```

Now move the patch.

```ucm
.> move.patch foo.patch qux.patch
```

The moved patch should be gone.

```ucm
.> view.patch foo.patch
```
