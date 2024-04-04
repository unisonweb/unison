```ucm
runtime-tests/selected> run.native tests

  ()

runtime-tests/selected> run.native tests.jit.only

  ()

```
Per Dan:
It's testing a flaw in how we were sending code from a scratch file to the native runtime, when that happened multiple times.
Related to the verifiable refs and recursive functions.
```unison
foo = do
  go : Nat ->{Exception} ()
  go = cases
    0 -> ()
    n -> go (decrement n)
  go 1000
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : '{Exception} ()

```
```ucm
.> run.native foo

  ()

.> run.native foo

  ()

```
