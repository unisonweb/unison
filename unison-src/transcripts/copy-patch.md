# Test that copying a patch works as expected

```unison:hide
x = 1
```

```ucm
.> add
```

Change the definition of `x` so something goes in our patch:

```unison:hide
x = 2
```

```ucm
.> update.old foo.patch
```

Copy the patch and make sure it's still there.

```ucm
.> copy.patch foo.patch bar.patch
.> ls foo
.> view.patch foo.patch
.> ls bar
.> view.patch bar.patch
```

Now move the patch.

```ucm
.> move.patch foo.patch qux.patch
```

The moved patch should be gone.

```ucm:error
.> view.patch foo.patch
.> ls foo
```
