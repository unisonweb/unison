# Ensure Test watch dependencies are properly considered.

https://github.com/unisonweb/unison/issues/2195

```unison
x = 999
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat

```
```unison
x = 1000
test> mytest = [let x + 1 == 1001; Ok "ok"]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      mytest : [Result]
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    2 | test> mytest = [let x + 1 == 1001; Ok "ok"]
    
    ✅ Passed ok

```
