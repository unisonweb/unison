
File for test cases making sure that universal equality/comparison
cases exist for built-in types. Just making sure they don't crash.

```unison
unique type A = A

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
    
      unique type A
      threadEyeDeez : ∀ _. _ ->{IO} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type A
    threadEyeDeez : ∀ _. _ ->{IO} ()

.> run threadEyeDeez

```
```unison
> typeLink A == typeLink A
> typeLink Text == typeLink Text
> typeLink Text == typeLink A
> termLink threadEyeDeez == termLink threadEyeDeez
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > typeLink A == typeLink A
          ⧩
          true
  
    2 | > typeLink Text == typeLink Text
          ⧩
          true
  
    3 | > typeLink Text == typeLink A
          ⧩
          false
  
    4 | > termLink threadEyeDeez == termLink threadEyeDeez
          ⧩
          true

```
