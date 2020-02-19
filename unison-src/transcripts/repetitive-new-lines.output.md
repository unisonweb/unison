##### Steps to reproduce the "new line on update" bug

```unison
f: Nat -> Nat
f x = 0

test> path.to.one = check (f 1 == 2)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      f           : Nat -> Nat
      path.to.one : [Result]
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    4 | test> path.to.one = check (f 1 == 2)
    
    🚫 FAILED 

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    f           : Nat -> Nat
    path.to.one : [Result]

.> test

  Cached test results (`help testcache` to learn more)
  
  ✗ path.to.one   
  
  🚫 1 test(s) failing
  
  Tip: Use view path.to.one to view the source of a test.

```
```unison
f x = x + 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      f : Nat -> Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> update

  ⍟ I've updated to these definitions:
  
    f : base.Nat -> base.Nat

.> test

  ✅  

  

  

    New test results:
  
  ◉ path.to.one    : Proved.
  
  ✅ 1 test(s) passing
  
  Tip: Use view path.to.one to view the source of a test.

```
