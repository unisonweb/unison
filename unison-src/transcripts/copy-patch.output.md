# Test that copying a patch works as expected

```unison
x = 1
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    x : ##Nat

```
Change the definition of `x` so something goes in our patch:

```unison
x = 2
```

```ucm
.> update foo.patch

  ⍟ I've updated these names to your new definition:
  
    x : ##Nat

```
Copy the patch and make sure it's still there.

```ucm
.> copy.patch foo.patch bar.patch

  Done.

.> ls foo

  1. patch (patch)

.> view.patch foo.patch

  Edited Terms: 1. #gjmq673r1v -> 2. x
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.> ls bar

  1. patch (patch)

.> view.patch bar.patch

  Edited Terms: 1. #gjmq673r1v -> 2. x
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

```
Now move the patch.

```ucm
.> move.patch foo.patch qux.patch

  Done.

```
The moved patch should be gone.

```ucm
.> view.patch foo.patch

  This patch is empty.

.> ls foo

  nothing to show

```
