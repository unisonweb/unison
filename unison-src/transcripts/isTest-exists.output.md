This transcript tests that UCM can always access the definition of 
`IsTest`/`isTest`, which is used internally.

```ucm
.> builtins.merge

  Done.

```
```unison
test> pass = [Ok "Passed"]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      pass : [Result]
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | test> pass = [Ok "Passed"]
    
    ✅ Passed Passed

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    pass : [Result]

.> links pass

  1. builtin.metadata.isTest : IsTest
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.> display 1

  IsTest

```
The definition and type of `isTest should exist.
