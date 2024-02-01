
File for test cases making sure that universal equality/comparison
cases exist for built-in types. Just making sure they don't crash.

```ucm:hide
.> builtins.mergeio
```

```unison
unique type A = A

threadEyeDeez _ =
  t1 = forkComp '()
  t2 = forkComp '()
  (t1 == t2, t1 < t2)
```

```ucm
.> add
.> run threadEyeDeez
```

```unison
> typeLink A == typeLink A
> typeLink Text == typeLink Text
> typeLink Text == typeLink A
> termLink threadEyeDeez == termLink threadEyeDeez
```
