# Block on required update

Should block an `update` if it requires an update on an in-file dependency.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
x = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      x : Nat
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Update `x`, and add a new `y` which depends on the update

``` unison
x = 10
y = x + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      y : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat
```

Try to add only the new `y`. This should fail because it requires an update to `x`, but we only ran an 'add'.

``` ucm :error
scratch/main> add y

  ⚠️

  Sorry, I wasn’t sure how to process your request:

    I expected no arguments, but received one.

  You can run `help add` for more information on using `update`.
```
