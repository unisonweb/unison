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
              lit1 = 1
              v2 = Nat.drop x1 lit1
              lit2 = 0
              v3 = Nat.eq v2 lit2
              v4 =
                match v3 with
                  false -> eval' v2
                  _ ->
                    lit3 = 1
                    lit3
              x1 Nat.* v4
            lit = 0
            v1 = Nat.eq x lit
            match v1 with
              false -> eval' x
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
              v4 = ##List.viewl x1
              match v4 with
                VElem x2 rest1 ->
                  v5 = fn x2
                  v6 = acc :+ v5
                  map$go' v6 rest1
                VEmpty -> acc
            fn x3 =
              lit = 2
              v7 = x3 Nat.* lit
              lit1 = 1
              v7 Nat.+ lit1
            v1 = ##List.viewl xs
            match v1 with
              VElem x rest ->
                v2 = fn x
                v3 = c :+ v2
                map$go' v3 rest
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
            evalK' x1 k =
              lit4 = 1
              v4 = Nat.drop x1 lit4
              lit5 = 0
              v5 = Nat.eq v4 lit5
              match v5 with
                false ->
                  clo1 = partialApply evalK$Lambda2$Lambda0 k x1
                  evalK' v4 clo1
                _ ->
                  lit6 = 1
                  v6 = x1 Nat.* lit6
                  k v6
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
                    clo = partialApply evalK$Lambda2$Lambda0' x
                    evalK' v2 clo
                  _ ->
                    lit3 = 1
                    x Nat.* lit3
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
              lit1 = 1
              v2 = Nat.lt x1 lit1
              match v2 with
                false ->
                  lit2 = 1
                  r4 = Nat.drop x1 lit2
                  lit3 = 0
                  v3 = Nat.eq r4 lit3
                  match v3 with
                    false ->
                      r5 = evalA' r4
                      match r5 with
                        Right p2 ->
                          match p2 with
                            Cons r6 rest2 ->
                              r7 = x1 Nat.* r6
                              nil = ()
                              p3 = Cons r7 nil
                              Right p3
                        Left done2 -> Left done2
                    _ ->
                      r8 = evalA'1 r4
                      match r8 with
                        Right p4 ->
                          match p4 with
                            Cons r9 rest3 ->
                              r10 = x1 Nat.* r9
                              nil1 = ()
                              p5 = Cons r10 nil1
                              Right p5
                        Left done3 -> Left done3
                _ ->
                  con = None
                  Left con
            evalA'1 x2 =
              lit4 = 1
              nil2 = ()
              p6 = Cons lit4 nil2
              Right p6
            lit = 0
            v1 = Nat.eq x lit
            match v1 with
              false ->
                r = evalA' x
                match r with
                  Right p ->
                    match p with
                      Cons r1 rest -> Some r1
                  Left done -> done
              _ ->
                r2 = evalA'1 x
                match r2 with
                  Right p1 ->
                    match p1 with
                      Cons r3 rest1 -> Some r3
                  Left done1 -> done1
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

A handler stays when something in its scope could perform its requests unseen, here a call
of an unknown function; the code under it is still specialized:

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
            handled f =
              v1 = St.get
              v2 = f v1
              _ = put v2
              St.get
            fn s v3 =
              match v3 with
                {St.get} ->
                  cont1 = shift {St}
                  clo1 = partialApply fn s
                  handle {St}
                    cont1 s
                  with clo1
                {put s'} ->
                  cont2 = shift {St}
                  clo2 = partialApply fn1 s'
                  handle {St}
                    con = ()
                    cont2 con
                  with clo2
                {a} -> a
            fn1 s1 v4 =
              match v4 with
                {St.get} ->
                  cont3 = shift {St}
                  clo3 = partialApply fn1 s1
                  handle {St}
                    cont3 s1
                  with clo3
                {put s'1} ->
                  cont4 = shift {St}
                  clo4 = partialApply fn1 s'1
                  handle {St}
                    con1 = ()
                    cont4 con1
                  with clo4
                {a1} -> a1
            clo = partialApply fn s
            handle {St}
              handled f
            with clo
          """
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

A handler is fused around a loop only when every case that resumes re-installs the handler
as its last act. One whose case does more with what the resumption gives back, or resumes
twice, stays a handler, with the code under it specialized:

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
          #30uu2adem2 z n =
            handled z n =
              v1 = z Nat.+ n
              lit = 0
              v2 = Nat.eq v1 lit
              match v2 with
                false ->
                  _ = tick
                  lit1 = 1
                  v3 = Nat.drop v1 lit1
                  v4 = ticks' v3
                  v4 Nat.+ v1
                _ -> 0
            ticks' n1 =
              lit2 = 0
              v5 = Nat.eq n1 lit2
              match v5 with
                false ->
                  _1 = tick
                  lit3 = 1
                  v6 = Nat.drop n1 lit3
                  v7 = ticks' v6
                  v7 Nat.+ n1
                _ -> 0
            fn v8 =
              match v8 with
                {tick} ->
                  cont1 = shift {Tick}
                  p1 =
                    clo1 = partialApply fn
                    handle {Tick}
                      con = ()
                      cont1 con
                    with clo1
                  match p1 with
                    Cons n2 p2 ->
                      match p2 with
                        Cons a p3 ->
                          match p3 with
                            () ->
                              lit4 = 1
                              v9 = n2 Nat.+ lit4
                              (v9, a)
                {a1} ->
                  lit5 = 0
                  (lit5, a1)
            clo = partialApply fn
            handle {Tick}
              handled z n
            with clo
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
