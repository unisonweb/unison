# `ir` and `ir.optimized`

`ir` shows the intermediate code (ANF) the runtime makes of a term, and `ir.optimized` the same code after optimization.

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
fib : Nat -> Nat
fib n = if n < 2 then n else fib (drop n 1) + fib (drop n 2)

even : Nat -> Boolean
even x = if x == 0 then true else odd (drop x 1)

odd : Nat -> Boolean
odd x = if x == 0 then false else even (drop x 1)

addAll : Nat -> [Nat] -> [Nat]
addAll k xs = List.map (x -> x + k) xs

sumWith : Nat -> [Nat] -> Nat
sumWith k xs =
  go acc = cases
    [] -> acc
    x +: rest -> go (acc + x * k) rest
  go 0 xs

describe : Optional Nat -> Text
describe = cases
  None -> "none"
  Some 0 -> "zero"
  Some n -> "some " ++ Nat.toText n

ability Counter where
  bump : Nat

runCounter : '{Counter} a -> a
runCounter p =
  h n = cases
    { a } -> a
    { bump -> k } -> handle k n with h (n + 1)
  handle p() with h 0

type Pair = Pair Nat Nat

sumCount : [Nat] -> Nat
sumCount xs =
  go : (Nat, Nat) -> [Nat] -> Nat
  go st = cases
    [] -> match st with (acc, n) -> acc + n
    x +: rest -> match st with (acc, n) -> go (acc + x, n + 1) rest
  go (0, 0) xs

divMod : Nat -> Nat -> (Nat, Nat)
divMod a b = if a < b then (0, a) else match divMod (drop a b) b with (q, r) -> (q + 1, r)

useDivMod : Nat -> Nat
useDivMod n = match divMod n 7 with (q, r) -> q + r

swapSum : Nat -> Nat -> Nat
swapSum a b = match (b, a) with (x, y) -> x + y

classify : Nat -> Nat
classify n =
  t = if n < 10 then Some n else None
  match t with
    Some m -> m + 1
    None -> 0
```

``` ucm :hide
scratch/main> add
```

Before optimization, `fib` calls the builtin functions; after, the calls are inlined as primitive instructions.

``` ucm
scratch/main> ir fib

  fib n =
    v1 = 2
    v2 = n < v1
    match v2 with
      false ->
        v3 = 1
        v4 = Nat.drop n v3
        v5 = fib v4
        v6 = 2
        v7 = Nat.drop n v6
        v8 = fib v7
        v5 Nat.+ v8
      _ -> n

scratch/main> ir.optimized fib

  fib n =
    v1 = 2
    v2 = prim.LESU n v1
    match v2 with
      false ->
        v3 = 1
        v4 = prim.DRPN n v3
        -- wide
        v5 = fib v4
        v6 = 2
        v7 = prim.DRPN n v6
        -- wide
        v8 = fib v7
        prim.ADDN v5 v8
      _ -> n
```

A term in a cycle shows the whole cycle.

``` ucm
scratch/main> ir even

  even x =
    v1 = 0
    v2 = x == v1
    match v2 with
      false ->
        v3 = 1
        v4 = Nat.drop x v3
        odd v4
      _ -> true

  odd x =
    v1 = 0
    v2 = x == v1
    match v2 with
      false ->
        v3 = 1
        v4 = Nat.drop x v3
        even v4
      _ -> false
```

Lambdas and local functions are lifted out into definitions of their own, named after where they came from.

``` ucm
scratch/main> ir addAll

  addAll k xs =
    v1 = addAll$Lambda0 k
    List.map v1 xs

  addAll$Lambda0 k x = x Nat.+ k

scratch/main> ir sumWith

  sumWith k xs =
    v1 = 0
    sumWith$go k v1 xs

  sumWith$go k acc x1 =
    v1 = ##List.viewl x1
    match v1 with
      VElem p1 p2 ->
        v2 = p1 Nat.* k
        v3 = acc Nat.+ v2
        sumWith$go k v3 p2
      VEmpty -> acc

scratch/main> ir describe

  describe x1 =
    match x1 with
      Some p1 ->
        match p1 with
          0 -> "zero"
          _ ->
            v1 = "some "
            v2 = Nat.toText p1
            v1 Text.++ v2
      None -> "none"
```

Optimization recognizes `runCounter`'s handler as affine and adds a second matcher for it.

``` ucm
scratch/main> ir runCounter

  runCounter p =
    v1 = 0
    v2 = runCounter$h v1
    v3 = runCounter$Lambda0 p  -- closure
    v2 v3

  runCounter$h n x1 =
    f1 n v2 =
      match v2 with
        {bump} ->
          shift {Counter} v3 ->
            p1 = ##jumpCont v3  -- closure
            v4 = 1
            v5 = n Nat.+ v4
            v6 = runCounter$h v5
            v7 = runCounter$h$Lambda0 p1 n  -- closure
            v6 v7
        {p2} -> p2
    v1 = f1 n  -- closure
    handle {Counter}
      x1()
    with v1

  runCounter$h$Lambda0 k n = k n

  runCounter$Lambda0 p =
    v1 = ()
    p v1

scratch/main> ir.optimized runCounter

  runCounter p =
    v1 = 0
    v2 = runCounter$Lambda0 p  -- closure
    runCounter$h v1 v2

  runCounter$h n x1 =
    f1 n v3 =
      match v3 with
        {bump} ->
          shift {Counter} v4 ->
            p1 = ##jumpCont v4  -- closure
            v5 = 1
            v6 = prim.ADDN n v5
            v1 = f1 v6  -- closure
            handle {Counter}
              p1 n
            with v1
        {p2} -> p2
    f2 n v7 v3 =
      match v3 with
        {bump} ->
          local v7 ->
            v5 = 1
            v6 = prim.ADDN n v5
            v1 = f2 v6 v7  -- closure
            _ = update v7 v1
            n
        {p2} -> p2
    v1 = f1 n  -- closure
    v2 = f2 n  -- closure
    handle {Counter}
      x1()
    with v1  -- affine: v2

  runCounter$h$Lambda0 k n = k n

  runCounter$Lambda0 p =
    v1 = ()
    p v1
```

The loader's rewrites of the optimized code show too. A loop whose state is a tuple gets a specialisation that takes the fields as arguments (`#spec1`):

``` ucm
scratch/main> ir.optimized sumCount

  sumCount xs =
    v1 = 0
    v2 = 0
    sumCount$go#spec1 v1 v2 xs

  sumCount$go st x1 =
    v1 = prim.VWLS x1
    match v1 with
      VElem p1 p2 ->
        match st with
          Cons p3 p4 ->
            match p4 with
              Cons p5 p6 ->
                match p6 with
                  () ->
                    v2 = prim.ADDN p3 p1
                    v3 = 1
                    v4 = prim.ADDN p5 v3
                    sumCount$go#spec1 v2 v4 p2
      VEmpty ->
        match st with
          Cons p7 p8 ->
            match p8 with
              Cons p9 p10 ->
                match p10 with
                  () -> prim.ADDN p7 p9

  sumCount$go#spec1 eta1 eta2 x1 =
    v1 = prim.VWLS x1
    match v1 with
      VElem p1 p2 ->
        v2 = prim.ADDN eta1 p1
        v3 = 1
        v4 = prim.ADDN eta2 v3
        sumCount$go#spec1 v2 v4 p2
      VEmpty -> prim.ADDN eta1 eta2
```

A function that returns a pair gets a worker that returns the two values without building it (`#return`): its tails `yield (# q, r #)`, several values rather than a pair. A caller that takes the pair apart calls it and binds the values, `(# q, r #) = ...`; the entry still builds the pair, for the callers that keep it.

``` ucm
scratch/main> ir.optimized divMod

  divMod a b =
    v1 = prim.LESU a b
    match v1 with
      false ->
        v2 = prim.DRPN a b
        (# eta1, eta2 #) = divMod#return v2 b
        v3 = 1
        v4 = prim.ADDN eta1 v3
        (v4, eta2)
      _ ->
        v7 = 0
        (v7, a)

  divMod#return a b =
    v1 = prim.LESU a b
    match v1 with
      false ->
        v2 = prim.DRPN a b
        (# eta1, eta2 #) = divMod#return v2 b
        v3 = 1
        v4 = prim.ADDN eta1 v3
        yield (# v4, eta2 #)
      _ ->
        v7 = 0
        yield (# v7, a #)

scratch/main> ir.optimized useDivMod

  useDivMod n =
    v1 = 7
    (# eta1, eta2 #) = divMod#return n v1
    prim.ADDN eta1 eta2
```

A match on a constructor built on the spot, or on the result of a match whose arms build constructors, is resolved where the constructor is known:

``` ucm
scratch/main> ir.optimized swapSum

  swapSum a b = prim.ADDN b a

scratch/main> ir.optimized classify

  classify n =
    v1 = 10
    v2 = prim.LESU n v1
    match v2 with
      false -> 0
      _ ->
        v3 = 1
        prim.ADDN n v3
```

Builtins have intermediate code too.

``` ucm
scratch/main> ir Nat.+

  (Nat.+) v1 v2 = prim.ADDN v1 v2
```

Constructors don't.

``` ucm
scratch/main> ir Pair.Pair

  Pair.Pair is a constructor, which has no intermediate code of
  its own.
```

``` ucm :error
scratch/main> ir nope

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    nope
```
