# Ensure test watch dependencies are properly considered.

https://github.com/unisonweb/unison/issues/2195

We add a simple definition.

```unison
x = 999
```

Now, we update that definition and define a test-watch which depends on it.

```unison
x = 1000
test> mytest = checks [x + 1 == 1001]
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

    2 | test> mytest = checks [x + 1 == 1001]
    
    ✅ Passed Passed

```
We expect this 'add' to fail because the test is blocked by the update to `x`.

```ucm
.> add

  x These definitions failed:
  
    Reason
    needs update   x        : Nat
    blocked        mytest   : [Result]
  
    Tip: Use `help filestatus` to learn more.

```
---

```unison
y = 42
test> useY = checks [y + 1 == 43]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      useY : [Result]
      y    : Nat
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    2 | test> useY = checks [y + 1 == 43]
    
    ✅ Passed Passed

```
This should correctly identify `y` as a dependency and add that too.

```ucm
.> add useY

  ⍟ I've added these definitions:
  
    useY : [Result]
    y    : Nat

```
