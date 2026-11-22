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
          #he1qjbhhqu σ2 σ3 σ4 =
            σ σ2 σ3 σ7 =
              σ8 = 1
              σ9 = Nat.drop σ7 σ8
              σ10 = 0
              σ11 = Nat.eq σ9 σ10
              σ12 =
                match σ11 with
                  false -> σ σ2 σ3 σ9
                  _ -> σ1 σ2 σ3 σ9
              σ7 Nat.* σ12
            σ1 σ2 σ3 σ13 = 1
            σ5 = 0
            σ6 = Nat.eq σ4 σ5
            match σ6 with
              false -> σ σ2 σ3 σ4
              _ -> σ1 σ2 σ3 σ4
          """
```
