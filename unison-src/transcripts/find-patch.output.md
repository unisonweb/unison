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
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

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
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      hey : Text
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Update

```ucm
.> update

  ⍟ I've updated to these definitions:
  
    hey : builtin.Text

.> find.patch

  1. patch

.> view.patch patch

  Edited Terms: hey#8e79 -> hey

.> view.patch 1

  Edited Terms: hey#8e79 -> hey

```
