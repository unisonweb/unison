# Ensure test watch dependencies are properly considered.

https://github.com/unisonweb/unison/issues/2195

We add a simple definition.

``` unison :hide
x = 999
```

``` ucm :hide
scratch/main> add
```

Now, we update that definition and define a test-watch which depends on it.

``` unison
x = 1000
test> mytest = checks [x + 1 == 1001]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      mytest : [Result]
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    2 | test> mytest = checks [x + 1 == 1001]
    
    ✅ Passed Passed
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

-----

``` unison
y = 42
test> useY = checks [y + 1 == 43]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      useY : [Result]
      y    : Nat

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    2 | test> useY = checks [y + 1 == 43]
    
    ✅ Passed Passed
```

This should correctly identify `y` as a dependency and add that too.

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
