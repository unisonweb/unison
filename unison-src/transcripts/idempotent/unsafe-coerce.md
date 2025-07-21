``` ucm :hide
scratch/main> builtins.merge
```

``` unison
f : '{} Nat
f _ = 5

fc : '{IO, Exception} Nat
fc = unsafe.coerceAbilities f

main : '{IO, Exception} [Result]
main _ =
  n = !fc
  if n == 5 then [Ok ""] else [Fail ""]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + f    : 'Nat
  + fc   : '{IO, Exception} Nat
  + main : '{IO, Exception} [Result]

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> find unsafe.coerceAbilities

  1. builtin.unsafe.coerceAbilities : (a ->{e1} b) -> a -> b

scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> io.test main

    New test results:

    1. main   ◉ 

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
