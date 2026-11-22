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

  Okay, I'm searching the branch for code that needs to be
  updated...

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
          -- closure of ReferenceDerived (Id "00dj5fj3l3kgqb63qutrc1m4ejs69c2qhincm4dg8gr808ecpif1u" 0) 0, captured:
          --   BLit (Pos 1)
          plus3 a b c =
            v1 = a Nat.+ b
            v1 Nat.+ c
          """

    2 | > runtime.ir 42
          ⧩
          """
          BLit (Pos 42)
          
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
