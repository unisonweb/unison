
```ucm:hide
.> builtins.merge
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
.> find unsafe.coerceAbilities
.> add
.> io.test main
```
