# `specialize`

`specialize f x` means `f x`. The result may be a different value, made of new code with `x`
folded in, but it behaves the same. See `unison-runtime/src/Unison/Runtime/Specialize.md`.

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

``` unison
plus3 : Nat -> Nat -> Nat -> Nat
plus3 a b c = a + b + c

ability Tell where
  tell : Nat -> ()

runTell : '{Tell} a -> ([Nat], a)
runTell p =
  go acc = cases
    { a } -> (acc, a)
    { tell n -> k } -> handle k () with go (List.snoc acc n)
  handle p () with go []

adder : Nat ->{Tell} (Nat -> Nat)
adder n =
  tell n
  x -> x + n

roundTrip : a ->{IO, Exception} a
roundTrip x =
  match Value.deserialize (Value.serialize (Value.value x)) with
    Left e -> bug e
    Right v -> match Value.load v with
      Left _ -> bug "missing code"
      Right a -> a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ability Tell

  + adder     : Nat ->{Tell} Nat -> Nat
  + plus3     : Nat -> Nat -> Nat -> Nat
  + roundTrip : a ->{IO, Exception} a
  + runTell   : '{Tell} a -> ([Nat], a)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

The result means the same as `f x`, whatever arity `f` has.

``` unison
> specialize plus3 1 2 3
> (specialize plus3 1) 2 3
> specialize (specialize plus3 1) 2 3
> specialize (x -> x + 1) 41
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize plus3 1 2 3
          ⧩
          6

    2 | > (specialize plus3 1) 2 3
          ⧩
          6

    3 | > specialize (specialize plus3 1) 2 3
          ⧩
          6

    4 | > specialize (x -> x + 1) 41
          ⧩
          42
```

A specialized closure displays as `specialize f x`.

``` unison
> specialize plus3 1
> specialize (specialize plus3 1) 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize plus3 1
          ⧩
          specialize (a b c -> a Nat.+ b Nat.+ c) 1

    2 | > specialize (specialize plus3 1) 2
          ⧩
          specialize
            (specialize (a b c -> a Nat.+ b Nat.+ c) 1) 2
```

Specializing the same closure twice gives equal values. A specialized closure need not equal
the closure it was made from.

``` unison
> (Universal.==) (specialize plus3 1) (specialize plus3 1)
> (Universal.==) (specialize plus3 1) (plus3 1)
> (Universal.==) (specialize plus3 1) (specialize plus3 2)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > (Universal.==) (specialize plus3 1) (specialize plus3 1)
          ⧩
          true

    2 | > (Universal.==) (specialize plus3 1) (plus3 1)
          ⧩
          false

    3 | > (Universal.==) (specialize plus3 1) (specialize plus3 2)
          ⧩
          false
```

`f x` runs, with its effects, when `f` takes one argument: here `adder 5` tells `5` once, and
the closure it returns is then specialized.

``` unison
> runTell do
    g = specialize adder 5
    (g 1, g 2)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > runTell do
          ⧩
          ([5], (6, 7))
```

A specialized closure survives serialization: it is written as the closure it was made from,
in version 5 of the value format, and loads back as the same specialized closure.

``` unison
main : '{IO, Exception} (Nat, Boolean, Boolean)
main = do
  g = specialize plus3 1
  h = roundTrip g
  sameBytes = (Universal.==) (Value.serialize (Value.value g)) (Value.serialize (Value.value (plus3 1)))
  (h 2 3, (Universal.==) h g, sameBytes)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + main : '{IO, Exception} (Nat, Boolean, Boolean)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run main

  (6, true, false)
```

A mutable value as the argument is shared, not copied.

``` unison
mutable : '{IO, Exception} Nat
mutable = do
  r = IO.ref 1
  g = specialize (r n -> Ref.read r + n) r
  Ref.write r 10
  g 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mutable : '{IO, Exception} Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run mutable

  15
```

`runtime.ir` shows the code the runtime runs for a closure.

``` unison
> runtime.ir (plus3 1)
> runtime.ir 42
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > runtime.ir (plus3 1)
          ⧩
          """
          -- b c -> 1 Nat.+ b Nat.+ c
          plus3 a b c =
            v1 = a Nat.+ b
            v1 Nat.+ c
          """

    2 | > runtime.ir 42
          ⧩
          """
          42
          
          """
```

`specialize.boundary f x` is `f x`.

``` unison
> specialize.boundary plus3 1 2 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize.boundary plus3 1 2 3
          ⧩
          6
```

## Partial evaluation

Specializing folds in what is known. `power 3` unrolls:

``` unison
power : Nat -> Nat -> Nat
power n x = if Nat.eq n 0 then 1 else x Nat.* power (Nat.drop n 1) x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + power : Nat -> Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> specialize power 3 2
> runtime.ir (specialize power 3)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize power 3 2
          ⧩
          8

    2 | > runtime.ir (specialize power 3)
          ⧩
          """
          -- specialize
          --   (n x -> (if Nat.eq n 0 then 1 else x Nat.* power (Nat.drop n 1) x)) 3
          #utqafuleos n x =
            lit = 1
            v1 = x Nat.* lit
            v2 = x Nat.* v1
            x Nat.* v2
          """
```

Specializing an interpreter on a program compiles the program (the first Futamura
projection). Here is an interpreter for a small language of numbered functions of one
argument:

``` unison
type Expr = Num Nat | Arg | Add Expr Expr | Sub Expr Expr | Mul Expr Expr
          | IfZero Expr Expr Expr | Call Nat Expr

eval : [Expr] -> Expr -> Nat -> Nat
eval prog e x = match e with
  Num n        -> n
  Arg          -> x
  Add a b      -> eval prog a x Nat.+ eval prog b x
  Sub a b      -> Nat.drop (eval prog a x) (eval prog b x)
  Mul a b      -> eval prog a x Nat.* eval prog b x
  IfZero c t f -> if Nat.eq (eval prog c x) 0 then eval prog t x else eval prog f x
  Call i a     -> match List.at i prog with
    Some body -> eval prog body (eval prog a x)
    None -> bug "no such function"

run : [Expr] -> Nat -> Nat
run prog = eval prog (Call 0 Arg)

factorial : [Expr]
factorial = [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Expr

  + eval      : [Expr] -> Expr -> Nat -> Nat
  + factorial : [Expr]
  + run       : [Expr] -> Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

An expression with no calls becomes straight-line code:

``` unison
> specialize (eval []) (Add (Mul Arg Arg) (Num 1)) 5
> runtime.ir (specialize (eval []) (Add (Mul Arg Arg) (Num 1)))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize (eval []) (Add (Mul Arg Arg) (Num 1)) 5
          ⧩
          26

    2 | > runtime.ir (specialize (eval []) (Add (Mul Arg Arg) (Num 1)))
          ⧩
          """
          -- specialize
          --   (e x -> (match e with
          --     Num n        -> n
          --     Arg          -> x
          --     Add a b      -> eval [] a x Nat.+ eval [] b x
          --     Sub a b      -> Nat.drop (eval [] a x) (eval [] b x)
          --     Mul a b      -> eval [] a x Nat.* eval [] b x
          --     IfZero c t f -> if Nat.eq (eval [] c x) 0 then eval [] t x else eval [] f x
          --     Call i a     ->
          --       match List.at i [] with
          --         Some body -> eval [] body (eval [] a x)
          --         None      -> bug "no such function")) (Add (Mul Arg Arg) (Num 1))
          #s7tck5o8q1 prog e x =
            v1 = x Nat.* x
            lit = 1
            v1 Nat.+ lit
          """
```

A recursive program becomes a recursive function, with no trace of the interpreter left:

``` unison
> specialize run factorial 5
> runtime.ir (specialize run factorial)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize run factorial 5
          ⧩
          120

    2 | > runtime.ir (specialize run factorial)
          ⧩
          """
          -- specialize
          --   (e x ->
          --     (match e with
          --       Num n -> n
          --       Arg -> x
          --       Add a b ->
          --         eval [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] a x
          --           Nat.+ eval
          --             [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] b x
          --       Sub a b ->
          --         Nat.drop
          --           (eval [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] a x)
          --           (eval [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] b x)
          --       Mul a b ->
          --         eval [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] a x
          --           Nat.* eval
          --             [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] b x
          --       IfZero c t f ->
          --         if
          --           Nat.eq
          --             (eval [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] c x)
          --             0 then
          --           eval [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] t x
          --         else eval [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] f x
          --       Call i a ->
          --         match List.at
          --           i [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] with
          --           Some body ->
          --             eval
          --               [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))]
          --               body
          --               (eval
          --                 [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))] a x)
          --           None -> bug "no such function"))
          --   (Call 0 Arg)
          #he1qjbhhqu prog e x =
            eval' x1 =
              lit2 = 0
              v4 = Nat.eq x1 lit2
              match v4 with
                false ->
                  lit3 = 1
                  v5 = Nat.drop x1 lit3
                  v6 = eval' v5
                  x1 Nat.* v6
                _ -> 1
            lit = 0
            v1 = Nat.eq x lit
            match v1 with
              false ->
                lit1 = 1
                v2 = Nat.drop x lit1
                v3 = eval' v2
                x Nat.* v3
              _ -> 1
          """
```

## Higher order

A known function argument is inlined into the loop that calls it:

``` unison
map : (a -> b) -> [a] -> [b]
map f xs =
  go acc = cases
    [] -> acc
    x +: rest -> go (List.snoc acc (f x)) rest
  go [] xs

foldLeft : (b -> a -> b) -> b -> [a] -> b
foldLeft f z = cases
  [] -> z
  x +: rest -> foldLeft f (f z x) rest
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foldLeft : (b ->{g1} a ->{g} b) -> b -> [a] ->{g, g1} b
  + map      : (a ->{g} b) -> [a] ->{g} [b]

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> specialize map (x -> x Nat.* 2 Nat.+ 1) [1, 2, 3]
> runtime.ir (specialize map (x -> x Nat.* 2 Nat.+ 1))
> specialize foldLeft (Nat.+) 0 [1, 2, 3]
> runtime.ir (specialize foldLeft (Nat.+))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize map (x -> x Nat.* 2 Nat.+ 1) [1, 2, 3]
          ⧩
          [3, 5, 7]

    2 | > runtime.ir (specialize map (x -> x Nat.* 2 Nat.+ 1))
          ⧩
          """
          -- specialize
          --   (f xs ->
          --     let
          --       go acc = cases
          --         []        -> acc
          --         x +: rest -> go (acc :+ f x) rest
          --       go [] xs) (x -> x Nat.* 2 Nat.+ 1)
          #gj43ai3imv f c c1 xs =
            map$go' acc x1 =
              v5 = ##List.viewl x1
              match v5 with
                VElem x2 rest1 ->
                  lit2 = 2
                  v6 = x2 Nat.* lit2
                  lit3 = 1
                  v7 = v6 Nat.+ lit3
                  v8 = acc :+ v7
                  map$go' v8 rest1
                VEmpty -> acc
            v1 = ##List.viewl xs
            match v1 with
              VElem x rest ->
                lit = 2
                v2 = x Nat.* lit
                lit1 = 1
                v3 = v2 Nat.+ lit1
                v4 = c :+ v3
                map$go' v4 rest
              VEmpty -> c1
          """

    3 | > specialize foldLeft (Nat.+) 0 [1, 2, 3]
          ⧩
          6

    4 | > runtime.ir (specialize foldLeft (Nat.+))
          ⧩
          """
          -- specialize
          --   (f z tqb401337t1 -> (match tqb401337t1 with
          --     []        -> z
          --     x +: rest -> foldLeft f (f z x) rest)) (Nat.+)
          #m3rbk3p595 f z x1 =
            foldLeft' z1 x2 =
              v3 = ##List.viewl x2
              match v3 with
                VElem x3 rest1 ->
                  v4 = z1 Nat.+ x3
                  foldLeft' v4 rest1
                VEmpty -> z1
            v1 = ##List.viewl x1
            match v1 with
              VElem x rest ->
                v2 = z Nat.+ x
                foldLeft' v2 rest
              VEmpty -> z
          """
```

An interpreter in continuation-passing style: each call nests the continuation deeper, so
what is known keeps growing. The specializer notices (the whistle) and passes the
continuation as an ordinary value instead, which leaves a factorial in continuation-passing
style:

``` unison
evalK : [Expr] -> Expr -> Nat -> (Nat -> Nat) -> Nat
evalK prog e x k = match e with
  Num n        -> k n
  Arg          -> k x
  Add a b      -> evalK prog a x (va -> evalK prog b x (vb -> k (va Nat.+ vb)))
  Sub a b      -> evalK prog a x (va -> evalK prog b x (vb -> k (Nat.drop va vb)))
  Mul a b      -> evalK prog a x (va -> evalK prog b x (vb -> k (va Nat.* vb)))
  IfZero c t f -> evalK prog c x (vc -> if Nat.eq vc 0 then evalK prog t x k else evalK prog f x k)
  Call i a     -> match List.at i prog with
    Some body -> evalK prog a x (va -> evalK prog body va k)
    None -> bug "no such function"

runK : [Expr] -> Nat -> Nat
runK prog x = evalK prog (Call 0 Arg) x (v -> v)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + evalK : [Expr]
            -> Expr
            -> Nat
            -> (Nat ->{g, g1} Nat)
            ->{g, g1} Nat
  + runK  : [Expr] -> Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> specialize runK factorial 5
> runtime.ir (specialize runK factorial)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize runK factorial 5
          ⧩
          120

    2 | > runtime.ir (specialize runK factorial)
          ⧩
          """
          -- specialize
          --   (prog x -> evalK prog (Call 0 Arg) x (v -> v))
          --   [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))]
          #2qmmp45ssf prog x =
            evalK$Lambda2$Lambda0' va vb = va Nat.* vb
            evalK$Lambda4' general va1 va2 =
              lit5 = 0
              v5 = Nat.eq va2 lit5
              match v5 with
                false ->
                  lit6 = 1
                  v6 = Nat.drop va2 lit6
                  clo1 = partialApply evalK$Lambda2$Lambda0 general va1
                  evalK$Lambda4' clo1 va2 v6
                _ ->
                  lit7 = 1
                  v7 = va1 Nat.* lit7
                  general v7
            lit = 0
            v1 = Nat.eq x lit
            match v1 with
              false ->
                lit1 = 1
                v2 = Nat.drop x lit1
                lit2 = 0
                v3 = Nat.eq v2 lit2
                match v3 with
                  false ->
                    lit3 = 1
                    v4 = Nat.drop v2 lit3
                    clo = partialApply evalK$Lambda2$Lambda0' x
                    evalK$Lambda4' clo v2 v4
                  _ ->
                    lit4 = 1
                    x Nat.* lit4
              _ -> 1
          """
```

## Abilities

Code that uses abilities is specialized too. Where everything is known it runs, whatever its
handler does with continuations: here `runSt 2 do count 4` becomes `12`.

``` unison
ability St where
  get : Nat
  put : Nat -> ()

runSt : Nat -> '{St} a -> a
runSt s p =
  go s = cases
    { a } -> a
    { St.get -> k } -> handle k s with go s
    { St.put s' -> k } -> handle k () with go s'
  handle p () with go s

count : Nat ->{St} Nat
count n = if Nat.eq n 0 then St.get else
  St.put (St.get Nat.+ n)
  count (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ability St

  + count : Nat ->{St} Nat
  + runSt : Nat -> '{St} a -> a

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> specialize (s x -> x Nat.+ runSt s do count 4) 2 100
> runtime.ir (specialize (s x -> x Nat.+ runSt s do count 4) 2)
> specialize (n -> runSt 0 do count n) 4
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize (s x -> x Nat.+ runSt s do count 4) 2 100
          ⧩
          112

    2 | > runtime.ir (specialize (s x -> x Nat.+ runSt s do count 4) 2)
          ⧩
          """
          -- specialize (s x -> x Nat.+ (runSt s do count 4)) 2
          #6patbfbgl6 s x =
            lit = 12
            x Nat.+ lit
          """

    3 | > specialize (n -> runSt 0 do count n) 4
          ⧩
          10
```

Where the handled code isn't all known, a known handler is fused away (below). An
interpreter that aborts on subtraction below zero, around a loop, becomes a loop that
returns an abort as a value, with no handler and no requests left:

``` unison
ability Abort where
  abort : Nat -> a

evalA : [Expr] -> Expr -> Nat ->{Abort} Nat
evalA prog e x = match e with
  Num n        -> n
  Arg          -> x
  Add a b      -> evalA prog a x Nat.+ evalA prog b x
  Sub a b      ->
    va = evalA prog a x
    vb = evalA prog b x
    if Nat.lt va vb then Abort.abort va else Nat.drop va vb
  Mul a b      -> evalA prog a x Nat.* evalA prog b x
  IfZero c t f -> if Nat.eq (evalA prog c x) 0 then evalA prog t x else evalA prog f x
  Call i a     -> match List.at i prog with
    Some body -> evalA prog body (evalA prog a x)
    None -> Abort.abort 999

runA : [Expr] -> Nat -> Optional Nat
runA prog x = handle evalA prog (Call 0 Arg) x with cases
  { a } -> Some a
  { Abort.abort _ -> _ } -> None

broken : [Expr]
broken = [Sub Arg (Num 10)]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ability Abort

  + broken : [Expr]
  + evalA  : [Expr] -> Expr -> Nat ->{Abort} Nat
  + runA   : [Expr] -> Nat -> Optional Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> specialize runA factorial 5
> specialize runA broken 3
> runtime.ir (specialize runA factorial)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize runA factorial 5
          ⧩
          Some 120

    2 | > specialize runA broken 3
          ⧩
          None

    3 | > runtime.ir (specialize runA factorial)
          ⧩
          """
          -- specialize
          --   (prog x ->
          --     (handle evalA prog (Call 0 Arg) x
          --     with cases
          --       { a }            -> Some a
          --       { abort _ -> _ } -> None))
          --   [IfZero Arg (Num 1) (Mul Arg (Call 0 (Sub Arg (Num 1))))]
          #0nklekfqfs prog x =
            evalA' x1 =
              lit4 = 0
              v3 = Nat.eq x1 lit4
              match v3 with
                false ->
                  lit5 = 1
                  v4 = Nat.lt x1 lit5
                  match v4 with
                    false ->
                      lit6 = 1
                      r4 = Nat.drop x1 lit6
                      r5 = evalA' r4
                      match r5 with
                        Right p1 ->
                          match p1 with
                            Cons r6 rest1 ->
                              r7 = x1 Nat.* r6
                              nil = ()
                              p2 = Cons r7 nil
                              Right p2
                        Left done1 -> Left done1
                    _ ->
                      con = None
                      Left con
                _ ->
                  lit7 = 1
                  nil1 = ()
                  p3 = Cons lit7 nil1
                  Right p3
            lit = 0
            v1 = Nat.eq x lit
            match v1 with
              false ->
                lit1 = 1
                v2 = Nat.lt x lit1
                match v2 with
                  false ->
                    lit2 = 1
                    r = Nat.drop x lit2
                    r1 = evalA' r
                    match r1 with
                      Right p ->
                        match p with
                          Cons r2 rest ->
                            r3 = x Nat.* r2
                            Some r3
                      Left done -> done
                  _ -> None
              _ ->
                lit3 = 1
                Some lit3
          """
```

A known handler is fused away where the specializer can see every request it handles: the
case for each request runs while specializing, and resuming the continuation carries on with
the rest of the body. State becomes plain values:

``` unison
stF : Nat -> Nat -> Nat
stF s n = runSt s do
  St.put (St.get Nat.+ n)
  St.put (St.get Nat.* 2)
  St.get
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + stF : Nat -> Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> specialize stF 1 5
> runtime.ir (specialize stF 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize stF 1 5
          ⧩
          12

    2 | > runtime.ir (specialize stF 1)
          ⧩
          """
          -- specialize
          --   (s n ->
          --     runSt s do
          --       use Nat * +
          --       use St get
          --       put (get + n)
          --       put (get * 2)
          --       get) 1
          #k8ughu55rj s n =
            v1 = s Nat.+ n
            lit = 2
            v1 Nat.* lit
          """
```

An abort becomes a branch, for an interpreted program with no loops:

``` unison
poly : [Expr]
poly = [Sub (Mul Arg Arg) (Num 10)]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + poly : [Expr]

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> specialize runA poly 5
> specialize runA poly 2
> runtime.ir (specialize runA poly)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize runA poly 5
          ⧩
          Some 15

    2 | > specialize runA poly 2
          ⧩
          None

    3 | > runtime.ir (specialize runA poly)
          ⧩
          """
          -- specialize
          --   (prog x ->
          --     (handle evalA prog (Call 0 Arg) x
          --     with cases
          --       { a }            -> Some a
          --       { abort _ -> _ } -> None)) [Sub (Mul Arg Arg) (Num 10)]
          #576qjmjsg7 prog x =
            va = x Nat.* x
            lit = 10
            v1 = Nat.lt va lit
            match v1 with
              false ->
                lit1 = 10
                r = Nat.drop va lit1
                Some r
              _ -> None
          """
```

Around a loop, the state is passed to the loop and returned from it:

``` unison
> specialize (s n -> runSt s do count n) 0 4
> runtime.ir (specialize (s n -> runSt s do count n) 0)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize (s n -> runSt s do count n) 0 4
          ⧩
          10

    2 | > runtime.ir (specialize (s n -> runSt s do count n) 0)
          ⧩
          """
          -- specialize (s n -> runSt s do count n) 0
          #8bdou15p4k s n =
            count' n1 v4 =
              lit2 = 0
              v5 = Nat.eq n1 lit2
              match v5 with
                false ->
                  v6 = v4 Nat.+ n1
                  lit3 = 1
                  v7 = Nat.drop n1 lit3
                  count' v7 v6
                _ ->
                  p1 = (v4, v4)
                  Right p1
            lit = 0
            v1 = Nat.eq n lit
            match v1 with
              false ->
                v2 = s Nat.+ n
                lit1 = 1
                v3 = Nat.drop n lit1
                r = count' v3 v2
                match r with
                  Right p ->
                    match p with
                      Cons r1 rest ->
                        match rest with
                          Cons s1 rest1 -> r1
                  Left done -> done
              _ -> s
          """
```

A call of an unknown function in a fused handler's scope could perform its requests unseen.
The call is made under a real handler made of the fused handler's cases, the bridge. It takes
the state and gives it back with the call's result, so the code around the call stays fused:
the first `St.get` is the state itself, and the last one is the state the bridge gave back.
A handler whose cases do more than resume as their last act (one that resumes twice, say)
has no such bridge, and stays as it is around its body:

``` unison
> specialize (s f -> runSt s do St.put (f St.get) ; St.get) 3 (x -> x Nat.* 2)
> runtime.ir (specialize (s f -> runSt s do St.put (f St.get) ; St.get) 3)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > specialize (s f -> runSt s do St.put (f St.get) ; St.get) 3 (x -> x Nat.* 2)
          ⧩
          6

    2 | > runtime.ir (specialize (s f -> runSt s do St.put (f St.get) ; St.get) 3)
          ⧩
          """
          -- specialize
          --   (s f ->
          --     runSt s do
          --       use St get
          --       put (f get)
          --       get) 3
          #aqe3doh5bf s f =
            bridge' general request =
              match request with
                {St.get} ->
                  cont1 = shift {St}
                  bridge1 = partialApply bridge' general
                  handle {St}
                    cont1 general
                  with bridge1
                {put s'} ->
                  cont2 = shift {St}
                  con = ()
                  bridge2 = partialApply bridge' s'
                  handle {St}
                    cont2 con
                  with bridge2
                {r2} ->
                  p1 = (r2, general)
                  Right p1
            bridge = partialApply bridge' s
            r =
              handle {St}
                f s
              with bridge
            match r with
              Right p ->
                match p with
                  Cons r1 rest ->
                    match rest with
                      Cons s1 rest1 -> r1
              Left done -> done
          """
```

The same with a handler that doesn't resume, with a loop that calls the unknown function
each time round, and with the function making requests of its own. Each pair is the plain
call and the specialized one:

``` unison
optA : '{Abort} Nat -> Optional Nat
optA p = handle p () with cases
  { a } -> Some a
  { Abort.abort _ -> _ } -> None

sumCalls : (Nat ->{St} Nat) -> Nat -> Nat ->{St} Nat
sumCalls f i acc = if Nat.eq i 0 then acc Nat.+ St.get else
  St.put (St.get Nat.+ 1)
  sumCalls f (Nat.drop i 1) (acc Nat.+ f i)

viaAbort : () -> (Nat ->{Abort} Nat) -> Optional Nat
viaAbort u f = optA do f 1 Nat.+ 1

viaLoop : Nat -> (Nat ->{St} Nat) -> Nat -> Nat
viaLoop s f n = runSt s do sumCalls f n 0

doubling : Nat ->{St} Nat
doubling n =
  St.put (St.get Nat.* 2)
  St.get Nat.+ n

> (viaAbort () (n -> n Nat.+ 1), specialize viaAbort () (n -> n Nat.+ 1))
> (viaAbort () (n -> Abort.abort n), specialize viaAbort () (n -> Abort.abort n))
> (viaLoop 3 doubling 5, specialize viaLoop 3 doubling 5)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + doubling : Nat ->{St} Nat
  + optA     : '{Abort} Nat -> Optional Nat
  + sumCalls : (Nat ->{St} Nat) -> Nat -> Nat ->{St} Nat
  + viaAbort : '((Nat ->{Abort} Nat) -> Optional Nat)
  + viaLoop  : Nat -> (Nat ->{St} Nat) -> Nat -> Nat

  Run `update` to apply these changes to your codebase.

    22 | > (viaAbort () (n -> n Nat.+ 1), specialize viaAbort () (n -> n Nat.+ 1))
           ⧩
           (Some 3, Some 3)

    23 | > (viaAbort () (n -> Abort.abort n), specialize viaAbort () (n -> Abort.abort n))
           ⧩
           (None, None)

    24 | > (viaLoop 3 doubling 5, specialize viaLoop 3 doubling 5)
           ⧩
           (473, 473)
```

A handler's pure case can use the state, and gets the state the loop gave back:

``` unison
runSt2 : Nat -> '{St} a -> (a, Nat)
runSt2 s p =
  go s = cases
    { a } -> (a, s)
    { St.get -> k } -> handle k s with go s
    { St.put s' -> k } -> handle k () with go s'
  handle p () with go s

count7 : Nat ->{St} Nat
count7 n = if Nat.eq n 0 then 7 else
  St.put (St.get Nat.+ n)
  count7 (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + count7 : Nat ->{St} Nat
  + runSt2 : Nat -> '{St} a -> (a, Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> (runSt2 0 do count7 4, specialize (s n -> runSt2 s do count7 n) 0 4)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > (runSt2 0 do count7 4, specialize (s n -> runSt2 s do count7 n) 0 4)
          ⧩
          ((7, 10), (7, 10))
```

A loop passes the handler's state when every case that resumes re-installs the handler as
its last act. Under a handler whose case does more with what the resumption gives back, or
resumes twice, the loop becomes a function that takes the rest of the body as a parameter
and gives the `handle`'s value:

``` unison
ability Tick where
  tick : ()

runTick : '{Tick} a -> (Nat, a)
runTick p =
  go = cases
    { a } -> (0, a)
    { tick -> k } -> match handle k () with go with
      (n, a) -> (n Nat.+ 1, a)
  handle p () with go

ticks : Nat ->{Tick} Nat
ticks n = if Nat.eq n 0 then 0 else
  tick
  ticks (Nat.drop n 1) Nat.+ n

ability Flip where
  flip : Boolean

allOf : '{Flip} a -> [a]
allOf p = handle p () with cases
  { a } -> [a]
  { flip -> k } -> allOf '(k true) List.++ allOf '(k false)

coins : Nat -> Nat ->{Flip} Nat
coins acc n = if Nat.eq n 0 then acc else
  coins (if flip then acc Nat.+ n else acc) (Nat.drop n 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ability Flip
  + ability Tick

  + allOf   : '{Flip} a -> [a]
  + coins   : Nat -> Nat ->{Flip} Nat
  + runTick : '{Tick} a -> (Nat, a)
  + ticks   : Nat ->{Tick} Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> (runTick do ticks 5, specialize (z n -> runTick do ticks (z Nat.+ n)) 0 5)
> (allOf do coins 0 3, specialize (z n -> allOf do coins z n) 0 3)
> runtime.ir (specialize (z n -> runTick do ticks (z Nat.+ n)) 0)
> runtime.ir (specialize (z n -> allOf do coins z n) 0)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > (runTick do ticks 5, specialize (z n -> runTick do ticks (z Nat.+ n)) 0 5)
          ⧩
          ((5, 15), (5, 15))

    2 | > (allOf do coins 0 3, specialize (z n -> allOf do coins z n) 0 3)
          ⧩
          ([6, 5, 4, 3, 3, 2, 1, 0], [6, 5, 4, 3, 3, 2, 1, 0])

    3 | > runtime.ir (specialize (z n -> runTick do ticks (z Nat.+ n)) 0)
          ⧩
          """
          -- specialize (z n -> runTick do ticks (z Nat.+ n)) 0
          #h48da7gja7 z n =
            resume v1 r1 =
              r2 = r1 Nat.+ v1
              lit5 = 0
              (lit5, r2)
            ticks' n2 k1 =
              lit6 = 0
              v5 = Nat.eq n2 lit6
              match v5 with
                false ->
                  lit7 = 1
                  v6 = Nat.drop n2 lit7
                  k2 = partialApply resume1 n2 k1
                  r3 = ticks' v6 k2
                  match r3 with
                    Cons n3 p3 ->
                      match p3 with
                        Cons a1 p4 ->
                          match p4 with
                            () ->
                              lit8 = 1
                              v7 = n3 Nat.+ lit8
                              (v7, a1)
                _ ->
                  lit9 = 0
                  k1 lit9
            resume1 n2 k1 r4 =
              r5 = r4 Nat.+ n2
              k1 r5
            v1 = z Nat.+ n
            lit = 0
            v2 = Nat.eq v1 lit
            match v2 with
              false ->
                lit1 = 1
                v3 = Nat.drop v1 lit1
                k = partialApply resume v1
                r = ticks' v3 k
                match r with
                  Cons n1 p1 ->
                    match p1 with
                      Cons a p2 ->
                        match p2 with
                          () ->
                            lit2 = 1
                            v4 = n1 Nat.+ lit2
                            (v4, a)
              _ ->
                lit3 = 0
                lit4 = 0
                (lit4, lit3)
          
          #h48da7gja7#return z n =
            v1 = z Nat.+ n
            lit = 0
            v2 = Nat.eq v1 lit
            match v2 with
              false ->
                lit1 = 1
                v3 = Nat.drop v1 lit1
                k = partialApply resume v1
                r = ticks' v3 k
                match r with
                  Cons n1 p1 ->
                    match p1 with
                      Cons a p2 ->
                        match p2 with
                          () ->
                            lit2 = 1
                            v4 = n1 Nat.+ lit2
                            yield (# v4, a #)
              _ ->
                lit3 = 0
                lit4 = 0
                yield (# lit4, lit3 #)
          """

    4 | > runtime.ir (specialize (z n -> allOf do coins z n) 0)
          ⧩
          """
          -- specialize (z n -> allOf do coins z n) 0
          #4laqa1niau z n =
            resume z n r2 =
              match r2 with
                false ->
                  lit1 = 1
                  v2 = Nat.drop n lit1
                  k = partialApply resume1
                  lit2 = 0
                  v3 = Nat.eq v2 lit2
                  match v3 with
                    false ->
                      con2 = true
                      r3 = resume2 z v2 k con2
                      con3 = false
                      r4 = resume2 z v2 k con3
                      r3 List.++ r4
                    _ -> k z
                _ ->
                  r5 = z Nat.+ n
                  lit3 = 1
                  v4 = Nat.drop n lit3
                  k1 = partialApply resume1
                  lit4 = 0
                  v5 = Nat.eq v4 lit4
                  match v5 with
                    false ->
                      con4 = true
                      r6 = resume2 r5 v4 k1 con4
                      con5 = false
                      r7 = resume2 r5 v4 k1 con5
                      r6 List.++ r7
                    _ -> k1 r5
            resume1 r8 = List.empty r8
            -- calling convention: acc:UN n1:BX k2:BX r9:BX
            resume2 acc n1 k2 r9 =
              match r9 with
                false ->
                  lit5 = 1
                  v6 = Nat.drop n1 lit5
                  lit6 = 0
                  v7 = Nat.eq v6 lit6
                  match v7 with
                    false ->
                      con6 = true
                      r10 = resume2 acc v6 k2 con6
                      con7 = false
                      r11 = resume2 acc v6 k2 con7
                      r10 List.++ r11
                    _ -> k2 acc
                _ ->
                  r12 = acc Nat.+ n1
                  lit7 = 1
                  v8 = Nat.drop n1 lit7
                  lit8 = 0
                  v9 = Nat.eq v8 lit8
                  match v9 with
                    false ->
                      con8 = true
                      r13 = resume2 r12 v8 k2 con8
                      con9 = false
                      r14 = resume2 r12 v8 k2 con9
                      r13 List.++ r14
                    _ -> k2 r12
            lit = 0
            v1 = Nat.eq n lit
            match v1 with
              false ->
                con = true
                r = resume z n con
                con1 = false
                r1 = resume z n con1
                r List.++ r1
              _ -> List.empty z
          """
```

A continuation that is resumed twice, or needed in another function, is made a residual
function of the request's result and the handler's state, and resuming it is a call:

``` unison
twoTicks : Nat ->{Tick} Nat
twoTicks n =
  tick
  tick
  n Nat.+ 1

twoFlips : Nat ->{Flip} Nat
twoFlips n =
  a = if flip then n else 0
  b = if flip then 10 else 20
  a Nat.+ b
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + twoFlips : Nat ->{Flip} Nat
  + twoTicks : Nat ->{Tick} Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

``` unison
> (runTick do twoTicks 5, specialize (z n -> runTick do twoTicks (z Nat.+ n)) 0 5)
> (allOf do twoFlips 3, specialize (z n -> allOf do twoFlips (z Nat.+ n)) 0 3)
> runtime.ir (specialize (z n -> allOf do twoFlips (z Nat.+ n)) 0)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > (runTick do twoTicks 5, specialize (z n -> runTick do twoTicks (z Nat.+ n)) 0 5)
          ⧩
          ((2, 6), (2, 6))

    2 | > (allOf do twoFlips 3, specialize (z n -> allOf do twoFlips (z Nat.+ n)) 0 3)
          ⧩
          ([13, 23, 10, 20], [13, 23, 10, 20])

    3 | > runtime.ir (specialize (z n -> allOf do twoFlips (z Nat.+ n)) 0)
          ⧩
          """
          -- specialize (z n -> allOf do twoFlips (z Nat.+ n)) 0
          #eealnqaafh z n =
            resume v1 r2 =
              match r2 with
                false ->
                  lit = 0
                  con2 = true
                  r3 = resume1 lit con2
                  con3 = false
                  r4 = resume1 lit con3
                  r3 List.++ r4
                _ ->
                  con4 = true
                  r5 = resume1 v1 con4
                  con5 = false
                  r6 = resume1 v1 con5
                  r5 List.++ r6
            -- calling convention: n1:UN r7:BX
            resume1 n1 r7 =
              match r7 with
                false ->
                  lit1 = 20
                  r8 = n1 Nat.+ lit1
                  List.empty r8
                _ ->
                  lit2 = 10
                  r9 = n1 Nat.+ lit2
                  List.empty r9
            v1 = z Nat.+ n
            con = true
            r = resume v1 con
            con1 = false
            r1 = resume v1 con1
            r List.++ r1
          """
```

## Mutable values

A specialization's code doesn't depend on which mutable value a closure holds, so closures
that differ only in their mutable values share it, while each keeps its own: specializing
`readPlus` on two refs, and on one of them a thousand more times, generates its code once.
Constants taken out of a mutable value (the ref in a pair, here) are each closure's own.

``` unison
readPlus : Ref {IO} Nat -> Nat ->{IO} Nat
readPlus r n = Ref.read r Nat.+ n

pairPlus : (Ref {IO} Nat, Nat) -> Nat ->{IO} Nat
pairPlus p n = match p with (r, k) -> Ref.read r Nat.+ k Nat.+ n

specializeMany : Nat -> Ref {IO} Nat -> ()
specializeMany i r = if Nat.eq i 0 then () else
  _ = specialize readPlus r
  specializeMany (Nat.drop i 1) r

mutableCases : '{IO, Exception} (Nat, Nat, Boolean, Boolean, Nat, Nat)
mutableCases = do
  r1 = IO.ref 1
  r2 = IO.ref 100
  f1 = specialize readPlus r1
  f1' = specialize readPlus r1
  f2 = specialize readPlus r2
  Ref.write r1 10
  specializeMany 1000 r1
  g1 = specialize pairPlus (r1, 5)
  g2 = specialize pairPlus (r2, 5)
  (f1 1, f2 1, (Universal.==) f1 f1', (Universal.==) f1 f2, g1 1, g2 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mutableCases   : '{IO, Exception} ( Nat,
                       Nat,
                       Boolean,
                       Boolean,
                       Nat,
                       Nat)
  + pairPlus       : (Ref {IO} Nat, Nat) -> Nat ->{IO} Nat
  + readPlus       : Ref {IO} Nat -> Nat ->{IO} Nat
  + specializeMany : Nat -> Ref {IO} Nat -> ()

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> run mutableCases

  (11, 101, true, false, 16, 106)
```
