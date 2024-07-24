
```ucm:hide
scratch/main> builtins.merge
```

```unison
f : '{} Nat
f _ = 5

fc : '{IO, Exception} Nat
fc = unsafe.coerceAbilities f

main : '{IO, Exception} [Result]
main _ =
  n = !fc
  if n == 5 then [Ok ""] else [Fail ""]
```

```ucm
scratch/main> find unsafe.coerceAbilities
scratch/main> add
scratch/main> io.test main
```
