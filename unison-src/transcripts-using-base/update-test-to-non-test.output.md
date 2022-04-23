When updating a term from a test to a non-test, we don't delete its metadata that indicates it's a test. This is a bug.

```unison
test> foo = []
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : [Result]
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | test> foo = []
    

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    foo : [Result]

```
```unison
foo = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    foo : Nat

.> links foo

  1. builtin.metadata.isTest : IsTest
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

```
