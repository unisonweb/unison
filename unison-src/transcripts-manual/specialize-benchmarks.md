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

-- higher-order code: functions passed to a loop

foldL : (b -> a -> b) -> b -> [a] -> b
foldL f z = cases
  [] -> z
  x +: rest -> foldL f (f z x) rest

sumSquaresByHand : Nat -> [Nat] -> Nat
sumSquaresByHand acc = cases
  [] -> acc
  x +: rest -> sumSquaresByHand (acc + x * x) rest

-- a filter and a map fused by hand into a fold, both functions arguments
sumWhere : (Nat -> Boolean) -> (Nat -> Nat) -> [Nat] -> Nat
sumWhere p f xs = foldL (acc x -> if p x then acc + f x else acc) 0 xs

sumWhereByHand : Nat -> [Nat] -> Nat
sumWhereByHand acc = cases
  [] -> acc
  x +: rest -> sumWhereByHand (if Nat.mod x 3 == 0 then acc + (x * 2 + 1) else acc) rest

-- a known list of functions, applied in turn
applyAll : [Nat -> Nat] -> Nat -> Nat
applyAll fs x = foldL (acc f -> f acc) x fs

stages : [Nat -> Nat]
stages = [x -> x + 1, x -> x * 3, x -> Nat.mod x 1000003, x -> x + 7, x -> x * x]

stagesByHand : Nat -> Nat
stagesByHand x =
  y = Nat.mod ((x + 1) * 3) 1000003 + 7
  y * y

-- the interpreter in continuation-passing style
evalK : [Expr] -> Expr -> Nat -> (Nat -> Nat) -> Nat
evalK prog e x k = match e with
  Num n        -> k n
  Arg          -> k x
  Add a b      -> evalK prog a x (va -> evalK prog b x (vb -> k (va + vb)))
  Sub a b      -> evalK prog a x (va -> evalK prog b x (vb -> k (va - vb)))
  Mul a b      -> evalK prog a x (va -> evalK prog b x (vb -> k (va * vb)))
  IfZero c t f -> evalK prog c x (vc -> if vc == 0 then evalK prog t x k else evalK prog f x k)
  Call i a     -> match List.at i prog with
    Some body -> evalK prog a x (va -> evalK prog body va k)
    None -> bug "no such function"

runK : [Expr] -> Nat -> Nat
runK prog x = evalK prog (Call 0 Arg) x (v -> v)

-- abilities: a state handler around a loop, and an interpreter that bails out

ability Cell where
  getCell : Nat
  putCell : Nat -> ()

runCell : Nat -> '{Cell} a -> a
runCell s p =
  go s = cases
    { a } -> a
    { Cell.getCell -> k } -> handle k s with go s
    { Cell.putCell s' -> k } -> handle k () with go s'
  handle p () with go s

countCell : Nat ->{Cell} Nat
countCell n = if n == 0 then Cell.getCell else
  Cell.putCell (Cell.getCell + n)
  countCell (n - 1)

countByHand : Nat -> Nat -> Nat
countByHand acc n = if n == 0 then acc else countByHand (acc + n) (n - 1)

ability Bail where
  bail : Nat -> a

evalB : [Expr] -> Expr -> Nat ->{Bail} Nat
evalB prog e x = match e with
  Num n        -> n
  Arg          -> x
  Add a b      -> evalB prog a x + evalB prog b x
  Sub a b      ->
    va = evalB prog a x
    vb = evalB prog b x
    if va < vb then Bail.bail va else va - vb
  Mul a b      -> evalB prog a x * evalB prog b x
  IfZero c t f -> if evalB prog c x == 0 then evalB prog t x else evalB prog f x
  Call i a     -> match List.at i prog with
    Some body -> evalB prog body (evalB prog a x)
    None -> Bail.bail 999

runB : [Expr] -> Nat -> Optional Nat
runB prog x = handle evalB prog (Call 0 Arg) x with cases
  { a } -> Some a
  { Bail.bail _ -> _ } -> None

-- fib with a guarded subtraction, so that the interpreter could bail
fibBProg : [Expr]
fibBProg =
  [IfZero Arg (Num 0)
    (IfZero (Sub Arg (Num 1)) (Num 1)
      (Add (Call 0 (Sub Arg (Num 1))) (Call 0 (Sub (Sub Arg (Num 1)) (Num 1)))))]

-- a handler whose case goes on after resuming, and one that resumes twice,
-- each around a loop

ability Tick where
  tick : ()

runTick : '{Tick} a -> (Nat, a)
runTick p =
  go = cases
    { a } -> (0, a)
    { Tick.tick -> k } -> match handle k () with go with
      (n, a) -> (n + 1, a)
  handle p () with go

ticks : Nat ->{Tick} Nat
ticks n = if n == 0 then 0 else
  Tick.tick
  ticks (n - 1) + n

ticksByHand : Nat -> (Nat, Nat)
ticksByHand n = if n == 0 then (0, 0) else
  match ticksByHand (n - 1) with
    (c, a) -> (c + 1, a + n)

ability Flip where
  flip : Boolean

allOf : '{Flip} a -> [a]
allOf p = handle p () with cases
  { a } -> [a]
  { Flip.flip -> k } -> allOf '(k true) List.++ allOf '(k false)

coins : Nat -> Nat ->{Flip} Nat
coins acc n = if n == 0 then acc else
  coins (if Flip.flip then acc + n else acc) (n - 1)

coinsByHand : Nat -> Nat -> [Nat]
coinsByHand acc n = if n == 0 then [acc] else
  coinsByHand (acc + n) (n - 1) List.++ coinsByHand acc (n - 1)

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
  foldSpec : Nat -> [Nat] -> Nat
  foldSpec = specialize foldL (acc x -> acc + x * x)
  printTime "foldLeft with a lambda, 10000 elements: generic" 1 (n -> repeat n do foldL (acc x -> acc + x * x) 0 xs)
  printTime "foldLeft with a lambda, 10000 elements: specialized" 1 (n -> repeat n do foldSpec 0 xs)
  printTime "foldLeft with a lambda, 10000 elements: by hand" 1 (n -> repeat n do sumSquaresByHand 0 xs)
  whereSpec = specialize (sumWhere (x -> Nat.mod x 3 == 0)) (x -> x * 2 + 1)
  printTime "filter and map as a fold, 10000 elements: generic" 1 (n -> repeat n do sumWhere (x -> Nat.mod x 3 == 0) (x -> x * 2 + 1) xs)
  printTime "filter and map as a fold, 10000 elements: specialized" 1 (n -> repeat n do whereSpec xs)
  printTime "filter and map as a fold, 10000 elements: by hand" 1 (n -> repeat n do sumWhereByHand 0 xs)
  stagesSpec = specialize applyAll stages
  printTime "five functions from a list, 100000 points: generic" 1 (n -> repeat n do sumOver (applyAll stages) 100000)
  printTime "five functions from a list, 100000 points: specialized" 1 (n -> repeat n do sumOver stagesSpec 100000)
  printTime "five functions from a list, 100000 points: by hand" 1 (n -> repeat n do sumOver stagesByHand 100000)
  countSpec : Nat -> Nat
  countSpec = specialize (s n -> runCell s do countCell n) 0
  printTime "state handler around a loop, 10000 rounds: generic" 1 (n -> repeat n do runCell 0 do countCell 10000)
  printTime "state handler around a loop, 10000 rounds: specialized" 1 (n -> repeat n do countSpec 10000)
  printTime "state handler around a loop, 10000 rounds: by hand" 1 (n -> repeat n do countByHand 0 10000)
  fibBSpec = specialize runB fibBProg
  printTime "fib 20, interpreter with an ability to bail: interpreted" 1 (n -> repeat n do runB fibBProg 20)
  printTime "fib 20, interpreter with an ability to bail: specialized" 1 (n -> repeat n do fibBSpec 20)
  ticksSpec : Nat -> (Nat, Nat)
  ticksSpec = specialize (z n -> runTick do ticks (z + n)) 0
  printTime "tick counter around a recursion, 1000 deep: generic" 1 (n -> repeat n do runTick do ticks 1000)
  printTime "tick counter around a recursion, 1000 deep: specialized" 1 (n -> repeat n do ticksSpec 1000)
  printTime "tick counter around a recursion, 1000 deep: by hand" 1 (n -> repeat n do ticksByHand 1000)
  coinsSpec : Nat -> [Nat]
  coinsSpec = specialize (z n -> allOf do coins z n) 0
  printTime "coin flips in a loop, 10 flips: generic" 1 (n -> repeat n do allOf do coins 0 10)
  printTime "coin flips in a loop, 10 flips: specialized" 1 (n -> repeat n do coinsSpec 10)
  printTime "coin flips in a loop, 10 flips: by hand" 1 (n -> repeat n do coinsByHand 0 10)
  fibKSpec = specialize runK fibProg
  printTime "fib 20, interpreter in CPS: interpreted" 1 (n -> repeat n do runK fibProg 20)
  printTime "fib 20, interpreter in CPS: specialized" 1 (n -> repeat n do fibKSpec 20)
```

``` ucm
jit-tests/main> run specializeSuite
```
