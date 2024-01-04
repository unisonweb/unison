# find.patch Test

```unison
---
title: test.u
---
hey = "yello"

```


```ucm

  Loading changes detected in test.u.

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

  Loading changes detected in test.u.

  I found and typechecked these definitions in test.u. If you do
  an `add` or `update`, here's how your codebase would change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      hey : Text

```
Update

```ucm
.> update.old

  ⍟ I've updated these names to your new definition:
  
    hey : Text

.> find.patch

  1. patch

.> view.patch 1

  Edited Terms: 1. #m0kuh98ou7 -> 2. hey
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

```
