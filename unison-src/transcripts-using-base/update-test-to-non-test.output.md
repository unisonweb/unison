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
.> update.old

  ⍟ I've updated these names to your new definition:
  
    foo : Nat

.> links foo

```



🛑

The transcript failed due to an error in the stanza above. The error is:

⚠️
I don't know how to links. Type `help` or `?` to get help.
