
Note: This should be forked off of the codebase created by base.md

If you want to define more complex tests somewhere other than `tests.u`, just `load my-tests.u` then `add`,
then reference those tests (which should be of type `'{IO,Exception,Tests} ()`, written using calls
to `Tests.check` and `Tests.checkEqual`).

```ucm
.> run.native tests

  ()

```
```ucm
.> run.native tests.jit.only

  ()

```
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
