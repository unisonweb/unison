
File for test cases making sure that universal equality/comparison
cases exist for built-in types. Just making sure they don't crash.

```unison
threadEyeDeez _ =
  t1 = forkComp '()
  t2 = forkComp '()
  t1 == t2 
  t1 < t2
  ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      threadEyeDeez : ∀ _. _ ->{IO} ()

```
```ucm
.> run threadEyeDeez

```
