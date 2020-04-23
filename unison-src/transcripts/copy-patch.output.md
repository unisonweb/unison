# Test that copying a patch works as expected

```unison
x = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : ##Nat

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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : ##Nat

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

.> view.patch foo.patch

  Edited Terms: x#jk19sm5bf8 -> x
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.> view.patch bar.patch

  Edited Terms: x#jk19sm5bf8 -> x
  
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

```
