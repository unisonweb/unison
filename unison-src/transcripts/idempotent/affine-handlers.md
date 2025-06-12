``` ucm :hide
scratch/main> builtins.merge
```

This transcript gives some examples of affine handlers and tests that
their performance is being improved. Handlers can be optimized if
all their branches fall into two cases:

1.  The continuation is unused in the branch
2.  The continuation is used exactly once in tail position, handled
    with the same handler.

It's allowed to mix these cases together, and do some initial
branching, so long as the branches ultimately fall into one of these
two cases.

Also, the optimized version of a handler can only be used if the
context it's installed only contains other optimized handlers. If a
non-optimized handler is already on the stack, the ordinary version of
handlers below it must be used.

``` unison
ability Repeat where
  times : Nat -> ()

looped : '{g} r -> Nat ->{g} ()
looped th = cases
  0 -> ()
  n ->
    _ = th ()
    looped th (Nat.drop n 1)

repeated : Request {Repeat, g} () ->{g} ()
repeated = cases
  { r } -> r
  { times n -> k } ->
    handle looped k n with repeated

now : '{IO, Exception} TimeSpec
now _ = match monotonic () with
  Left e -> raise e
  Right t -> t

elapsed : TimeSpec -> TimeSpec -> Float
elapsed t0 t1 =
  n0 = nsec t0
  s0 = sec t0

  n1 = nsec t1
  s1 = sec t1

  dn = Nat.sub n1 n0
  ds = s1 - s0

  Int.toFloat ds + Int.toFloat dn * 1e-9

-- Tests that the thunk is at least twice as fast in an affine context
testPerf : '() ->{IO, Exception} Result
testPerf th =
  t0 = now ()
  th ()
  t1 = now ()
  handle
    times 1
    th ()
  with repeated
  t2 = now ()

  dt0 = elapsed t0 t1
  dt1 = elapsed t1 t2

  ratio = dt1 / dt0

  if ratio > 2.0
  then Ok "performance improved"
  else Fail ("performance too similar: " ++ Float.toText ratio)

ability Count where
  tick : Nat

ability Env e where
  ask : e

provide : e -> Request {Env e} r ->{g} r
provide e = cases
  { r } -> r
  { ask -> k } -> handle k e with provide e

-- Affine handler directly operating on a Request
counter'ugly : Nat -> Request {Count} r -> r
counter'ugly n = cases
  { r } -> r
  { tick -> k } ->
    handle k n
    with counter'ugly (n+1)

-- Affine handler using thunks for a somewhat nicer form
counter'nice : Nat -> '{Count} r -> r
counter'nice n th = handle !th with cases
  { r } -> r
  { tick -> k } ->
    counter'nice (n+1) do k n

count'loop : Nat ->{Count} ()
count'loop = cases
  0 -> ()
  n ->
    _ = tick
    count'loop (Nat.drop n 1)

-- This is a loop with as many iterations as the first argument, with
-- as many irrelevant handlers installed as the second argument. The
-- irrelevant handlers will take up much more time if the context is
-- not affine.
count'wrap : Nat -> Nat ->{Count} ()
count'wrap k = cases
  0 -> count'loop k
  n -> handle count'wrap k (Nat.drop n 1) with provide 0

count'test = do
  [ testPerf do handle count'wrap 1000 100 with counter'ugly 0
  , testPerf do counter'nice 0 do count'wrap 1000 100
  ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      ability Count
      ability Env e
      ability Repeat
      count'loop   : Nat ->{Count} ()
      count'test   : '{IO, Exception} [Result]
      count'wrap   : Nat -> Nat ->{Count} ()
      counter'nice : Nat -> '{Count} r -> r
      counter'ugly : Nat -> Request {Count} r -> r
      elapsed      : TimeSpec -> TimeSpec -> Float
      looped       : '{g} r -> Nat ->{g} ()
      now          : '{IO, Exception} TimeSpec
      provide      : e -> Request {Env e} r -> r
      repeated     : Request {Repeat, g} () ->{g} ()
      testPerf     : '() ->{IO, Exception} Result
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> io.test count'test

    New test results:

    1. count'test   ◉ performance improved
                    ◉ performance improved

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

The following test illustrates using both sort of handler cases.
`fail` is never called in the test, but the optimization is only
looking at handlers, not the effectful code.

``` unison
ability CountOrFail where
  tock : Nat
  fail : e

fail'counter : Nat -> '{CountOrFail} r -> ()
fail'counter n th = handle th () with cases
  { r } -> ()
  { tock -> k } -> fail'counter (n+1) do k n
  { fail -> _ } -> ()

fail'count'loop : Nat ->{CountOrFail} ()
fail'count'loop = cases
  0 -> ()
  n ->
    _ = tock
    fail'count'loop (Nat.drop n 1)

fail'count'wrap : Nat -> Nat ->{CountOrFail} ()
fail'count'wrap k = cases
  0 -> fail'count'loop k
  n -> handle fail'count'wrap k (Nat.drop n 1) with provide 0

fail'count'test = do
  [ testPerf do fail'counter 1000 do fail'count'wrap 1000 100 ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      ability CountOrFail
      fail'count'loop : Nat ->{CountOrFail} ()
      fail'count'test : '{IO, Exception} [Result]
      fail'count'wrap : Nat -> Nat ->{CountOrFail} ()
      fail'counter    : Nat -> '{CountOrFail} r -> ()
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> io.test fail'count'test

    New test results:

    1. fail'count'test   ◉ performance improved

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

This tests a case where affine handlers were not being optimized due
to overly restrictive criteria for recognizing them. In this case,
because n \< o, parts of the handler will end up with the n and o
variables in different orders. I.E. an 'entry point' will end up with
`o n -> ...` because the `o` gets prepended, but the matching cases
will end up with `n o -> ...` because its argument list is generated
based completely on the order in which free variables are sorted by
name.

This has nothing to do with the handler being affine, and was just an
overly restrictive assumption about the exact form that handlers end
up in in the intermediate code. It shouldn't even matter if some
variables in the entry are unused in the matcher, although I'm
uncertain if that can happen. So, checking the relationship between
the argument list of the matcher and the entry point has just been
removed.

``` unison
local'counter : Nat -> '{Count} r -> r
local'counter o th =
  h n = cases
    { tick -> k } ->
      handle k (n + o) with h (n+1)
    { r } -> r
  handle !th with h 0

local'count'test = do
  [ testPerf do local'counter 5 do count'wrap 1000 100 ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      local'count'test : '{IO, Exception} [Result]
      local'counter    : Nat -> '{Count} r -> r
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> io.test local'count'test

    New test results:

    1. local'count'test   ◉ performance improved

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
