File for test cases making sure that universal equality/comparison
cases exist for built-in types. Just making sure they don't crash.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
unique type A = A

threadEyeDeez _ =
  t1 = forkComp '()
  t2 = forkComp '()
  (t1 == t2, t1 < t2)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      type A
      threadEyeDeez : ∀ _. _ ->{IO} (Boolean, Boolean)
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run threadEyeDeez

  (false, true)
```

``` unison
> typeLink A == typeLink A
> typeLink Text == typeLink Text
> typeLink Text == typeLink A
> termLink threadEyeDeez == termLink threadEyeDeez
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

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
