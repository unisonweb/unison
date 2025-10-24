``` ucm :hide
> builtins.merge
```

This transcript just runs the profiler with to-file output, to make
sure it doesn't crash.

``` unison
loop : Nat -> ()
loop = cases
  0 -> ()
  n -> loop (Nat.drop n 1)

loopTest : '()
loopTest = do loop 1000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + loop     : Nat -> ()
  + loopTest : '()

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> run.profiled.full loopTest loop.prof

  ()
```
