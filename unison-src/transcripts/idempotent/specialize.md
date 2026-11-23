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
          #utqafuleos σ σ1 =
            σ2 = 1
            σ3 = σ1 Nat.* σ2
            σ4 = σ1 Nat.* σ3
            σ1 Nat.* σ4
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
          #s7tck5o8q1 σ σ1 σ2 =
            σ3 = σ2 Nat.* σ2
            σ4 = 1
            σ3 Nat.+ σ4
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
          #he1qjbhhqu σ1 σ2 σ3 =
            σ σ6 =
              σ7 = 1
              σ8 = Nat.drop σ6 σ7
              σ9 = 0
              σ10 = Nat.eq σ8 σ9
              σ12 =
                match σ10 with
                  false -> σ σ8
                  _ ->
                    σ11 = 1
                    σ11
              σ6 Nat.* σ12
            σ4 = 0
            σ5 = Nat.eq σ3 σ4
            match σ5 with
              false -> σ σ3
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
          #gj43ai3imv σ2 σ3 σ4 σ5 =
            σ σ11 σ12 =
              σ13 = ##List.viewl σ12
              match σ13 with
                VElem σ14 σ15 ->
                  σ16 = σ1 σ14
                  σ17 = σ11 :+ σ16
                  σ σ17 σ15
                VEmpty -> σ11
            σ1 σ18 =
              σ19 = 2
              σ20 = σ18 Nat.* σ19
              σ21 = 1
              σ20 Nat.+ σ21
            σ6 = ##List.viewl σ5
            match σ6 with
              VElem σ7 σ8 ->
                σ9 = σ1 σ7
                σ10 = σ3 :+ σ9
                σ σ10 σ8
              VEmpty -> σ4
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
          #m3rbk3p595 σ1 σ2 σ3 =
            σ σ8 σ9 =
              σ10 = ##List.viewl σ9
              match σ10 with
                VElem σ11 σ12 ->
                  σ13 = σ8 Nat.+ σ11
                  σ σ13 σ12
                VEmpty -> σ8
            σ4 = ##List.viewl σ3
            match σ4 with
              VElem σ5 σ6 ->
                σ7 = σ2 Nat.+ σ5
                σ σ7 σ6
              VEmpty -> σ2
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
          #2qmmp45ssf σ2 σ3 =
            σ σ12 σ13 = σ12 Nat.* σ13
            σ1 σ14 σ15 =
              σ16 = 1
              σ17 = Nat.drop σ14 σ16
              σ18 = 0
              σ19 = Nat.eq σ17 σ18
              match σ19 with
                false ->
                  σ20 = partialApply evalK$Lambda2$Lambda0 σ15 σ14
                  σ1 σ17 σ20
                _ ->
                  σ21 = 1
                  σ22 = σ14 Nat.* σ21
                  σ15 σ22
            σ4 = 0
            σ5 = Nat.eq σ3 σ4
            match σ5 with
              false ->
                σ6 = 1
                σ7 = Nat.drop σ3 σ6
                σ8 = 0
                σ9 = Nat.eq σ7 σ8
                match σ9 with
                  false ->
                    σ10 = partialApply σ σ3
                    σ1 σ7 σ10
                  _ ->
                    σ11 = 1
                    σ3 Nat.* σ11
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
          #6patbfbgl6 σ σ1 =
            σ2 = 12
            σ1 Nat.+ σ2
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
          #0nklekfqfs σ2 σ3 =
            σ σ16 =
              σ17 = 1
              σ18 = Nat.lt σ16 σ17
              match σ18 with
                false ->
                  σ19 = 1
                  σ20 = Nat.drop σ16 σ19
                  σ21 = 0
                  σ22 = Nat.eq σ20 σ21
                  match σ22 with
                    false ->
                      σ23 = σ σ20
                      match σ23 with
                        Right σ24 -> Right σ24
                        Left σ25 ->
                          match σ25 with
                            Cons σ26 σ27 ->
                              σ28 = σ16 Nat.* σ26
                              σ29 = ()
                              σ30 = Cons σ28 σ29
                              Left σ30
                    _ ->
                      σ31 = σ1 σ20
                      match σ31 with
                        Right σ32 -> Right σ32
                        Left σ33 ->
                          match σ33 with
                            Cons σ34 σ35 ->
                              σ36 = σ16 Nat.* σ34
                              σ37 = ()
                              σ38 = Cons σ36 σ37
                              Left σ38
                _ ->
                  σ39 = None
                  Right σ39
            σ1 σ40 =
              σ41 = 1
              σ42 = ()
              σ43 = Cons σ41 σ42
              Left σ43
            σ4 = 0
            σ5 = Nat.eq σ3 σ4
            match σ5 with
              false ->
                σ6 = σ σ3
                match σ6 with
                  Right σ7 -> σ7
                  Left σ8 ->
                    match σ8 with
                      Cons σ9 σ10 -> Some σ9
              _ ->
                σ11 = σ1 σ3
                match σ11 with
                  Right σ12 -> σ12
                  Left σ13 ->
                    match σ13 with
                      Cons σ14 σ15 -> Some σ14
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
          #k8ughu55rj σ σ1 =
            σ2 = σ Nat.+ σ1
            σ3 = 2
            σ2 Nat.* σ3
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
          #576qjmjsg7 σ σ1 =
            σ2 = σ1 Nat.* σ1
            σ3 = 10
            σ4 = Nat.lt σ2 σ3
            match σ4 with
              false ->
                σ5 = 10
                σ6 = Nat.drop σ2 σ5
                Some σ6
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
          #8bdou15p4k σ1 σ2 =
            σ σ15 σ16 =
              σ17 = 0
              σ18 = Nat.eq σ15 σ17
              match σ18 with
                false ->
                  σ19 = σ16 Nat.+ σ15
                  σ20 = 1
                  σ21 = Nat.drop σ15 σ20
                  σ22 = σ σ21 σ19
                  match σ22 with
                    Right σ23 -> Right σ23
                    Left σ24 ->
                      match σ24 with
                        Cons σ25 σ26 ->
                          match σ26 with
                            Cons σ27 σ28 ->
                              σ31 = (σ25, σ27)
                              Left σ31
                _ ->
                  σ34 = (σ16, σ16)
                  Left σ34
            σ3 = 0
            σ4 = Nat.eq σ2 σ3
            match σ4 with
              false ->
                σ5 = σ1 Nat.+ σ2
                σ6 = 1
                σ7 = Nat.drop σ2 σ6
                σ8 = σ σ7 σ5
                match σ8 with
                  Right σ9 -> σ9
                  Left σ10 ->
                    match σ10 with
                      Cons σ11 σ12 ->
                        match σ12 with
                          Cons σ13 σ14 -> σ11
              _ -> σ1
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
          #aqe3doh5bf σ2 σ3 =
            σ σ2 σ8 =
              match σ8 with
                {St.get} ->
                  cont1 = shift {St}
                  σ9 = partialApply σ σ2
                  handle {St}
                    cont1 σ2
                  with σ9
                {put σ10} ->
                  cont2 = shift {St}
                  σ11 = partialApply σ1 σ10
                  handle {St}
                    σ12 = ()
                    cont2 σ12
                  with σ11
                {σ13} -> σ13
            σ1 σ14 σ15 =
              match σ15 with
                {St.get} ->
                  cont3 = shift {St}
                  σ16 = partialApply σ1 σ14
                  handle {St}
                    cont3 σ14
                  with σ16
                {put σ17} ->
                  cont4 = shift {St}
                  σ18 = partialApply σ1 σ17
                  handle {St}
                    σ19 = ()
                    cont4 σ19
                  with σ18
                {σ20} -> σ20
            σ4 = partialApply σ σ2
            handle {St}
              σ5 = St.get
              σ6 = σ3 σ5
              σ7 = put σ6
              St.get
            with σ4
          """
```
