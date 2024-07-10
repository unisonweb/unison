See [this ticket](https://github.com/unisonweb/unison/issues/873); the point being, this shouldn't crash the runtime. :)

``` unison
(-) = builtin.Nat.sub
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      - : Nat -> Nat -> Int

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    - : Nat -> Nat -> Int

```
``` unison
baz x = x - 1
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      baz : Nat -> Int

```
