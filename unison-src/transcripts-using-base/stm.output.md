Loops that access a shared counter variable, accessed in transactions.
Some thread delaying is just accomplished by counting in a loop.

``` unison
count : Nat -> ()
count = cases
  0 -> ()
  n -> count (drop n 1)

inc : TVar Nat ->{io2.IO} Nat
inc v =
  atomically 'let
    x = TVar.read v
    TVar.write v (x+1)
    x

loop : '{io2.IO} Nat -> Nat -> Nat ->{io2.IO} Nat
loop grab acc = cases
  0 -> acc
  n ->
    m = !grab
    count (m*10)
    loop grab (acc+m) (drop n 1)

body : Nat -> TVar (Optional Nat) -> TVar Nat ->{io2.IO} ()
body k out v =
  n = loop '(inc v) 0 k
  atomically '(TVar.write out (Some n))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + body  : Nat -> TVar (Optional Nat) -> TVar Nat ->{IO} ()
  + count : Nat -> ()
  + inc   : TVar Nat ->{IO} Nat
  + loop  : '{IO} Nat -> Nat -> Nat ->{IO} Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Test case.

``` unison
spawn : Nat ->{io2.IO} Result
spawn k = let
  out1 = TVar.newIO None
  out2 = TVar.newIO None
  counter = atomically '(TVar.new 0)
  void (forkComp '(Right (body k out1 counter)))
  void (forkComp '(Right (body k out2 counter)))
  p = atomically 'let
    r1 = TVar.read out1
    r2 = TVar.swap out2 None
    match (r1, r2) with
      (Some m, Some n) -> (m, n)
      _ -> !STM.retry
  max = TVar.readIO counter
  match p with (m, n) ->
    sum : Nat
    sum = max * drop max 1 / 2
    if m+n == sum
    then Ok "verified"
    else Fail (display m n sum)

display : Nat -> Nat -> Nat -> Text
display m n s =
  "mismatch: " ++ toText m ++ " + " ++ toText n ++ " /= " ++ toText s

nats : [Nat]
nats = [8,10,11,14,16,18,20,23,25,30]

tests : '{io2.IO} [Result]
tests = '(map spawn nats)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + display : Nat -> Nat -> Nat -> Text
  + nats    : [Nat]
  + spawn   : Nat ->{IO} Result
  + tests   : '{IO} [Result]

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> io.test tests

    New test results:

    1. tests   ◉ verified
               ◉ verified
               ◉ verified
               ◉ verified
               ◉ verified
               ◉ verified
               ◉ verified
               ◉ verified
               ◉ verified
               ◉ verified

  ✅ 10 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
