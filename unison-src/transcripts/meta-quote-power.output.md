# Staged power: the canonical Oleg example

Oleg Kiselyov's classic MetaOCaml staged power function:

``` ocaml
let rec power n x =
  if n = 0 then .<1>.
  else if n mod 2 = 0 then .<square .~(power (n/2) x)>.
  else .<.~x * .~(power (n-1) x)>.
```

<https://okmij.org/ftp/meta-programming/tutorial/power.ml>

`power n x` is a Unison-level function that runs at quote-construction
time. Given a literal `n` and a quoted expression `x : meta.Term meta.TermF`,
it produces the quoted AST of `x^n`, unrolling the multiplications and
exploiting `square` when `n` is even — same shape, transcribed to Unison
quasiquote.

``` ucm :hide
scratch/main> builtins.mergeio
```

The staged power function. Each `[| ... |]` builds an AST node; each
`${ ... }` splices in the recursive result. The even-`n` branch uses
the standard squaring trick by binding `r = x^(n/2)` once and squaring
the resulting value at runtime, instead of unrolling two recursive
calls.

``` unison
power : Nat -> meta.Term meta.TermF -> meta.Term meta.TermF
power n x =
  if n == 0 then
    [| 1 |]
  else if (Nat.mod n 2) == 0 then
    let
      half = power ((Nat./) n 2) x
      [| ${half} Nat.* ${half} |]
  else
    [| ${x} Nat.* ${power (Nat.drop n 1) x} |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + power : Nat -> meta.Term TermF -> meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view power

  power : Nat -> meta.Term TermF -> meta.Term TermF
  power n x =
    use Nat /
    use TermF App Ref
    use meta.Term Term
    if n == 0 then [| 1 |]
    else
      if Nat.mod n 2 == 0 then
        half = power (n / 2) x
        Term
          (Set Tip)
          (Tm
            (App
              (Term (Set Tip) (Tm (App [| (Nat.*) |] half)))
              half))
      else
        Term
          (Set Tip)
          (Tm
            (App
              (Term (Set Tip) (Tm (App [| (Nat.*) |] x)))
              (power (Nat.drop n 1) x)))
```

Build the AST of `\y -> y^7`. The outer `[| y -> ... |]` introduces a
HOAS-bound `y`; the inner `[| y |]` inside the splice refers back to
that binder by name. `power 7 [| y |]` unrolls to
`y * (y * (square (square y)))` — three multiplications plus one
top-level multiply, instead of the naive seven.

``` unison
power7 : meta.Term meta.TermF
power7 = [| y -> ${power 7 [| y |]} |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + power7 : meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> view power7

  power7 : meta.Term TermF
  power7 =
    use meta.Term Term
    Term
      (Set Tip)
      (Tm
        (Lam (Term (Set Tip) (Abs (Name "y") (power 7 [| y |])))))
```

The pretty printer recovers the staged form via the `[| ... |]`
round-trip, so we can read the unrolled AST directly.

Compile it down to a real `Nat -> Nat`, run it on `2`, and confirm
`2^7 = 128`.

``` unison
runPower7 : '{IO} Either Text Nat
runPower7 _ = match Meta.store power7 with
  Left e -> Left e
  Right link ->
    f : Nat -> Nat
    f = Meta.eval link
    Right (f 2)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + runPower7 : '{IO} Either Text Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run runPower7

  Right 128
```

Cross-check against a naïve runtime `power'`:

``` unison
power' : Nat -> Nat -> Nat
power' n x =
  if n == 0 then 1
  else x Nat.* power' (Nat.drop n 1) x

agrees : '{IO} Either Text Boolean
agrees _ = match Meta.store power7 with
  Left e -> Left e
  Right link ->
    staged : Nat -> Nat
    staged = Meta.eval link
    Right (staged 2 == power' 7 2
        && staged 3 == power' 7 3
        && staged 10 == power' 7 10)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + agrees : '{IO} Either Text Boolean
  + power' : Nat -> Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> run agrees

  Right true
```
