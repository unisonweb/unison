# find.patch Test

```unison
---
title: test.u
---
hey = "yello"

```


```ucm

  I found and typechecked these definitions in test.u. If you do
  an `add` or `update`, here's how your codebase would change:
  
    ⍟ These new definitions are ok to `add`:
    
      hey : Text

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    hey : Text

```
Update

```unison
---
title: test.u
---
hey = "hello"

```


```ucm

  I found and typechecked these definitions in test.u. If you do
  an `add` or `update`, here's how your codebase would change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      hey : Text

```
Update

```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    hey : Text

.> find.patch

  1. patch

.> view.patch patch

  Edited Terms: hey#m0kuh98ou7 -> hey
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.> view.patch 1

  Edited Terms: hey#m0kuh98ou7 -> hey
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

```
