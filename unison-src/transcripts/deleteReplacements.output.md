# Deleting term and type replacements from patches

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
.> update

  ⍟ I've updated these names to your new definition:
  
    x : ##Nat

.> view.patch

  Edited Terms: x#jk19sm5bf8 -> x
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

```
```ucm
.> delete.term-replacement #jk19

  Done.

.> view.patch

  This patch is empty.

```
```unison
unique[a] type Foo = Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Foo

```
```unison
unique[b] type Foo = Foo | Bar
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type Foo

```
```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    unique type Foo

.> view.patch

  Edited Types: Foo#hsk1l8232e -> Foo
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

```
```ucm
.> delete.type-replacement #hsk1l8232e

  Done.

.> view.patch

  This patch is empty.

```
```unison
bar = 3
unique[aa] type bar = Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type bar
      bar : ##Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type bar
    bar : ##Nat

```
```unison
unique[bb] type bar = Foo | Bar
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique type bar

```
```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    unique type bar

.> view.patch

  Edited Types: bar#b1ct5ub6du -> bar
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.> delete.type-replacement #b1ct5ub6du

  Done.

.> view.patch

  This patch is empty.

```
