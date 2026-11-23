# `specialize` benchmarks

Run this against the `jit` testing codebase (`scripts/testing-codebase.sh jit` downloads it
once and prints its directory), which has the `jit-tests` project with the base library and
the timing helpers (`printTime`, `repeat`). Use an optimized build, with the JIT off and on:

```
stack build --work-dir .stack-work-opt
UNISON_JIT=off stack exec --work-dir .stack-work-opt unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork unison-src/transcripts-manual/specialize-benchmarks.md
UNISON_JIT=on stack exec --work-dir .stack-work-opt unison -- -C "$(scripts/testing-codebase.sh jit)" transcript.fork unison-src/transcripts-manual/specialize-benchmarks.md
```

Each program is measured three ways: interpreted by `eval`, specialized from `eval`, and
written by hand in Unison. Success is "specialized" close to "by hand". The specialization
happens before the timing starts.

The testing codebase's builtins predate `specialize`:

``` ucm
jit-tests/main> alias.term ##specialize specialize
```

``` unison
type Expr = Num Nat | Arg | Add Expr Expr | Sub Expr Expr | Mul Expr Expr
          | IfZero Expr Expr Expr | Call Nat Expr

eval : [Expr] -> Expr -> Nat -> Nat
eval prog e x = match e with
  Num n        -> n
  Arg          -> x
  Add a b      -> eval prog a x + eval prog b x
  Sub a b      -> eval prog a x - eval prog b x
  Mul a b      -> eval prog a x * eval prog b x
  IfZero c t f -> if eval prog c x == 0 then eval prog t x else eval prog f x
  Call i a     -> match List.at i prog with
    Some body -> eval prog body (eval prog a x)
    None -> bug "no such function"

runProg : [Expr] -> Nat -> Nat
runProg prog = eval prog (Call 0 Arg)

-- fib x = if x == 0 then 0 else if x - 1 == 0 then 1 else fib (x - 1) + fib (x - 2)
fibProg : [Expr]
fibProg =
  [IfZero Arg (Num 0)
    (IfZero (Sub Arg (Num 1)) (Num 1)
      (Add (Call 0 (Sub Arg (Num 1))) (Call 0 (Sub Arg (Num 2)))))]

fibByHand : Nat -> Nat
fibByHand x =
  if x == 0 then 0
  else if x - 1 == 0 then 1
  else fibByHand (x - 1) + fibByHand (x - 2)

-- a polynomial with no calls: 3x^3 + 2x^2 + x + 7, summed over 0 to n
polyExpr : Expr
polyExpr =
  Add (Mul (Num 3) (Mul Arg (Mul Arg Arg)))
    (Add (Mul (Num 2) (Mul Arg Arg)) (Add Arg (Num 7)))

polyByHand : Nat -> Nat
polyByHand x = 3 * (x * (x * x)) + (2 * (x * x) + (x + 7))

sumOver : (Nat -> Nat) -> Nat -> Nat
sumOver f n =
  go acc i = if i > n then acc else go (acc + f i) (i + 1)
  go 0 0

mapL : (a -> b) -> [a] -> [b]
mapL f xs =
  go acc = cases
    [] -> acc
    x +: rest -> go (acc :+ f x) rest
  go [] xs

specializeSuite : '{IO, Exception} ()
specializeSuite = do
  fibSpec = specialize runProg fibProg
  printTime "fib 20: interpreted" 1 (n -> repeat n do runProg fibProg 20)
  printTime "fib 20: specialized" 1 (n -> repeat n do fibSpec 20)
  printTime "fib 20: by hand" 1 (n -> repeat n do fibByHand 20)
  polySpec = specialize (eval []) polyExpr
  printTime "polynomial, 100000 points: interpreted" 1 (n -> repeat n do sumOver (eval [] polyExpr) 100000)
  printTime "polynomial, 100000 points: specialized" 1 (n -> repeat n do sumOver polySpec 100000)
  printTime "polynomial, 100000 points: by hand" 1 (n -> repeat n do sumOver polyByHand 100000)
  xs = List.range 0 10000
  mapSpec = specialize mapL (x -> x * 2 + 1)
  printTime "map with a lambda, 10000 elements: generic" 1 (n -> repeat n do mapL (x -> x * 2 + 1) xs)
  printTime "map with a lambda, 10000 elements: specialized" 1 (n -> repeat n do mapSpec xs)
```

``` ucm
jit-tests/main> run specializeSuite
```
