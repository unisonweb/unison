
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
.> run threadEyeDeez
```
