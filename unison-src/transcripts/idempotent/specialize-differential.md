# `specialize`, tested against the interpreter it specializes

`specialize f x` must behave as `f x`. This transcript generates programs in a small
language, specializes an interpreter on each, and compares running the program both ways.
A program known to the specializer gives residual code in that program's shape, so random
programs give varied residual code. See "Testing" in
`unison-runtime/src/Unison/Runtime/Specialize.md`.

The language is a dynamically typed lambda calculus over `Nat`, with two bounded loops and
with effects: `Perform`, `Handle`, and `Resume` in a handler's clause. The generator is
typed, so a generated program never fails with a type error, and its handler clauses use
their continuation in each of the ways a handler can: not at all, as the last act, with
more work after, twice, and inside a function. A second generator makes loops under one
handler or two with a request each round.

To try other seeds, change the arguments of `check` at the end (the first seed and how many)
and run the transcript.

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

``` unison
-- A dynamically typed lambda calculus over Nat, with effects and handlers.

type Ty = TN | TF Ty Ty | TK

type Bin = Add | Sub | Mul | Eq | Lt

type Expr
  = Lit Nat
  | Var Nat
  | Lam Ty Expr
  | App Expr Expr
  | Prim Bin Expr Expr
  | If Expr Expr Expr
  | Let Ty Expr Expr
  | Iter Expr Expr Expr
  | PrimRec Expr Expr Expr
  | Perform Nat Expr
  | Handle Handler Expr
  | Resume Expr Expr Expr
  | Call Expr

type Handler = Handler Expr [Optional Expr] Expr

ability Eff where
  perform : Nat -> Nat -> Nat

type Val = N Nat | F Env Expr | K (Nat -> Nat ->{Eff} Val)

-- The environment is a type of the interpreter's own and not a List, so
-- that the specializer sees its shape: a variable is found while
-- specializing, and what it finds can be a function or a continuation
-- that is known.
--
-- It ends with the function a Call calls, if the run was given one: a
-- function the specializer doesn't know, which can make requests.
type Env = Empty | Bind Val Env | Ext (Nat ->{Eff} Nat)

lookup : Nat -> Env -> Val
lookup i = cases
  Bind v rest -> if i == 0 then v else lookup (Nat.drop i 1) rest
  _ -> bug "unbound variable"

ext : Env -> Nat ->{Eff} Nat
ext env n = match env with
  Bind _ rest -> ext rest n
  Ext f -> f n
  Empty -> n

nat : Val -> Nat
nat = cases
  N n -> n
  _ -> bug "expected a Nat"

apply : Val -> Val ->{Eff} Val
apply f x = match f with
  F env body -> eval (Bind x env) body
  _ -> bug "expected a function"

prim : Bin -> Nat -> Nat -> Nat
prim op a b = match op with
  Add -> a + b
  Sub -> Nat.drop a b
  Mul -> Nat.mod (a * b) 1009
  Eq -> if a == b then 1 else 0
  Lt -> if a < b then 1 else 0

eval : Env -> Expr ->{Eff} Val
eval env = cases
  Lit n -> N n
  Var i -> lookup i env
  Lam _ body -> F env body
  App f x -> apply (eval env f) (eval env x)
  Prim op a b -> N (prim op (nat (eval env a)) (nat (eval env b)))
  If c t f -> if nat (eval env c) == 0 then eval env t else eval env f
  Let _ e body -> eval (Bind (eval env e) env) body
  Iter n f z ->
    fv = eval env f
    go k acc = if k == 0 then acc else go (Nat.drop k 1) (apply fv acc)
    go (Nat.mod (nat (eval env n)) 6) (eval env z)
  PrimRec n s z ->
    sv = eval env s
    zv = eval env z
    go k = if k == 0 then zv else apply (apply sv (N k)) (go (Nat.drop k 1))
    go (Nat.mod (nat (eval env n)) 6)
  Perform i e -> N (Eff.perform i (nat (eval env e)))
  Handle h body -> handleWith env h do eval env body
  Resume k v s -> match eval env k with
    K resume -> resume (nat (eval env v)) (nat (eval env s))
    _ -> bug "expected a continuation"
  Call e -> N (ext env (nat (eval env e)))

-- the pure clause sees [result, state]; an operation's clause
-- [argument, state, continuation]; an operation with no clause is forwarded
handleWith : Env -> Handler -> '{Eff} Val ->{Eff} Val
handleWith env h p = match h with
  Handler pureC clauses init ->
    go : Nat -> Request {Eff} Val ->{Eff} Val
    go s = cases
      { v } -> eval (Bind v (Bind (N s) env)) pureC
      { Eff.perform i a -> k } -> match List.at i clauses with
        Some (Some c) -> eval (Bind (N a) (Bind (N s) (Bind (K (x s' -> handle k x with go s')) env))) c
        -- performed out here, where the enclosing handler sees it
        _ ->
          v = Eff.perform i a
          handle k v with go s
    handle p () with go (nat (eval env init))

topH : Request {Eff} Val -> Val
topH = cases
  { v } -> v
  { Eff.perform i a -> k } -> handle k (a * 2 + i) with topH

run : Expr -> Nat -> Nat -> Nat
run prog a b = nat (handle apply (apply (eval Empty prog) (N a)) (N b) with topH)

-- the same, with a function for the program's Calls
runWith : Expr -> (Nat ->{Eff} Nat) -> Nat -> Nat -> Nat
runWith prog f a b = nat (handle apply (apply (eval (Ext f) prog) (N a)) (N b) with topH)

-- a function to call that makes a request
requesting : Nat ->{Eff} Nat
requesting n = Eff.perform (Nat.mod n 2) n + 1

-- Types, for generating and shrinking; the interpreter ignores them.

isN : [Ty] -> Expr -> Boolean
isN ctx e = typeOf ctx e == Some TN

allClauses : [Ty] -> [Optional Expr] -> Boolean
allClauses ctx = cases
  [] -> true
  None +: rest -> allClauses ctx rest
  Some c +: rest -> isN ctx c && allClauses ctx rest

typeOf : [Ty] -> Expr -> Optional Ty
typeOf ctx = cases
  Lit _ -> Some TN
  Var i -> List.at i ctx
  Lam t b -> match typeOf (t +: ctx) b with
    Some r -> Some (TF t r)
    None -> None
  App f x -> match (typeOf ctx f, typeOf ctx x) with
    (Some (TF a r), Some a') | a == a' -> Some r
    _ -> None
  Prim _ a b -> if isN ctx a && isN ctx b then Some TN else None
  If c t f ->
    if isN ctx c then
      match (typeOf ctx t, typeOf ctx f) with
        (Some a, Some b) | a == b -> Some a
        _ -> None
    else None
  Let t x b -> if typeOf ctx x == Some t then typeOf (t +: ctx) b else None
  Iter n f z ->
    if isN ctx n && typeOf ctx f == Some (TF TN TN) && isN ctx z then Some TN else None
  PrimRec n s z ->
    if isN ctx n && typeOf ctx s == Some (TF TN (TF TN TN)) && isN ctx z then Some TN else None
  Perform _ x -> if isN ctx x then Some TN else None
  Handle (Handler p cs i) b ->
    if isN ctx b && isN ctx i && isN (TN +: (TN +: ctx)) p && allClauses (TN +: (TN +: (TK +: ctx))) cs
    then Some TN
    else None
  Resume k v s ->
    if typeOf ctx k == Some TK && isN ctx v && isN ctx s then Some TN else None
  Call x -> if isN ctx x then Some TN else None

progTy : Ty
progTy = TF TN (TF TN TN)

-- Random programs.

ability Rand where
  below : Nat -> Nat

runRand : Nat -> '{Rand} a -> a
runRand seed p =
  go s = cases
    { a } -> a
    { below n -> k } ->
      s' = s * 6364136223846793005 + 1442695040888963407
      handle k (Nat.mod (Nat.shiftRight s' 33) n) with go s'
  handle p () with go (seed * 2654435761 + 1)

pick : [a] ->{Rand} Optional a
pick xs = if List.size xs == 0 then None else List.at (below (List.size xs)) xs

varsOf : Ty -> [Ty] -> [Nat]
varsOf t ctx =
  go i acc = cases
    [] -> acc
    u +: rest -> go (i + 1) (if u == t then List.snoc acc i else acc) rest
  go 0 [] ctx

genTy : Nat ->{Rand} Ty
genTy d = if d == 0 || below 2 == 0 then TN else TF (genTy (Nat.drop d 1)) (genTy (Nat.drop d 1))

gen : [Ty] -> Ty -> Nat ->{Rand} Expr
gen ctx t size =
  leaf _ = match pick (varsOf t ctx) with
    Some i | below 3 > 0 -> Var i
    _ -> match t with
      TF a r -> Lam a (gen (a +: ctx) r 0)
      _ -> Lit (below 5)
  if size == 0 then leaf () else match t with
    TN -> genN ctx size
    TF a r -> match below 5 with
      0 -> leaf ()
      1 -> If (gen ctx TN (size / 2)) (gen ctx t (size / 2)) (gen ctx t (size / 2))
      _ -> Lam a (gen (a +: ctx) r (Nat.drop size 1))
    TK -> leaf ()

genN : [Ty] -> Nat ->{Rand} Expr
genN ctx size =
  h = size / 2
  n = gen ctx TN h
  match below 12 with
    0 -> Lit (below 5)
    1 -> gen ctx TN 0
    2 ->
      op = match below 5 with
        0 -> Add
        1 -> Sub
        2 -> Mul
        3 -> Eq
        _ -> Lt
      Prim op n (gen ctx TN h)
    3 -> If n (gen ctx TN h) (gen ctx TN h)
    4 ->
      t = genTy 2
      Let t (gen ctx t h) (gen (t +: ctx) TN h)
    5 ->
      a = genTy 2
      App (gen ctx (TF a TN) h) (gen ctx a h)
    6 -> Iter n (gen ctx (TF TN TN) h) (gen ctx TN h)
    7 -> PrimRec n (gen ctx (TF TN (TF TN TN)) h) (gen ctx TN h)
    8 -> Perform (below 2) n
    _ ->
      clause _ = if below 4 == 0 then None else Some (genClause (TN +: (TN +: (TK +: ctx))) h)
      Handle (Handler (gen (TN +: (TN +: ctx)) TN h) [clause (), clause ()] (Lit (below 3))) n

-- An operation's clause, in one of the ways a handler can use its
-- continuation (at Var 2, with the argument at Var 0 and the state at Var 1).
genClause : [Ty] -> Nat ->{Rand} Expr
genClause ctx size =
  e _ = gen ctx TN size
  resume v s = Resume (Var 2) v s
  match below 6 with
    -- doesn't resume
    0 -> e ()
    -- resumes as its last act (the state never goes down, so that resuming
    -- twice while it is small stays bounded)
    1 -> resume (e ()) (Prim Add (Var 1) (e ()))
    -- resumes, then does more with the result
    2 -> Prim Add (resume (e ()) (Var 1)) (e ())
    3 -> Let TN (resume (e ()) (Prim Add (Var 1) (Lit 1))) (gen (TN +: ctx) TN size)
    -- resumes twice, while the state is small
    4 ->
      s' = Prim Add (Var 1) (Lit 1)
      If (Prim Lt (Var 1) (Lit 2))
        (resume (Var 0) (Var 1))
        (Prim Add (resume (Var 0) s') (resume (e ()) s'))
    -- resumes inside a function, applied once: the argument can't see the
    -- function (TK, a type nothing is generated at, stands in for it)
    _ ->
      Let (TF TN TN) (Lam TN (Resume (Var 3) (Var 0) (Var 2)))
        (App (Var 0) (gen (TK +: ctx) TN size))

genProg : Nat ->{Rand} Expr
genProg size = Lam TN (Lam TN (gen [TN, TN] TN size))

-- A loop under one handler or two, with a request each round: the shape
-- where the loop must become a function that is passed the handlers' states.
-- The clauses use their continuation in every way, so many of these loops
-- have work waiting after a resumption.
genLoopProg : Nat ->{Rand} Expr
genLoopProg size =
  h = size / 2
  ctx = [TN, TN]
  round c = match below 3 with
    0 -> Perform (below 2) (gen c TN h)
    1 -> Prim Add (Perform (below 2) (gen c TN h)) (gen c TN h)
    _ -> Prim Add (gen c TN h) (Perform (below 2) (Var 0))
  loop = match below 2 with
    0 -> Iter (Var (below 2)) (Lam TN (round (TN +: ctx))) (gen ctx TN h)
    _ -> PrimRec (Var (below 2)) (Lam TN (Lam TN (round (TN +: (TN +: ctx))))) (gen ctx TN h)
  handled body =
    clause _ = genClause (TN +: (TN +: (TK +: ctx))) h
    second = if below 3 == 0 then None else Some (clause ())
    Handle (Handler (gen (TN +: (TN +: ctx)) TN h) [Some (clause ()), second] (Lit (below 3))) body
  Lam TN (Lam TN (if below 2 == 0 then handled loop else handled (handled loop)))

-- Shrinking: smaller programs of the same type that still fail.

lmap : (a ->{e} b) -> [a] ->{e} [b]
lmap f = cases
  [] -> []
  x +: rest -> f x +: lmap f rest

replaceAt : Nat -> a -> [a] -> [a]
replaceAt i x xs = List.take i xs List.++ (x +: List.drop (i + 1) xs)

-- a node's children, and the node rebuilt from new ones; an absent clause
-- is a placeholder, rebuilt as absent
children : Expr -> ([Expr], [Expr] ->{} Expr)
children e =
  two : (Expr -> Expr -> Expr) -> Expr -> Expr -> ([Expr], [Expr] ->{} Expr)
  two f a b = ([a, b], cases
    [a', b'] -> f a' b'
    _ -> e)
  three : (Expr -> Expr -> Expr -> Expr) -> Expr -> Expr -> Expr -> ([Expr], [Expr] ->{} Expr)
  three f a b c = ([a, b, c], cases
    [a', b', c'] -> f a' b' c'
    _ -> e)
  match e with
    Lam t b -> ([b], cases
      [b'] -> Lam t b'
      _ -> e)
    App f x -> two App f x
    Prim op a b -> two (Prim op) a b
    If c t f -> three If c t f
    Let t x b -> two (Let t) x b
    Iter n f z -> three Iter n f z
    PrimRec n s z -> three PrimRec n s z
    Perform i x -> ([x], cases
      [x'] -> Perform i x'
      _ -> e)
    Handle (Handler p cs i) b ->
      slot = cases
        Some c -> c
        None -> Lit 0
      back old new = match old with
        Some _ -> Some new
        None -> None
      ([p, i, b] List.++ lmap slot cs, cases
        p' +: (i' +: (b' +: cs')) -> Handle (Handler p' (zipBack back cs cs') i') b'
        _ -> e)
    Resume k v s -> three Resume k v s
    Call x -> ([x], cases
      [x'] -> Call x'
      _ -> e)
    _ -> ([], _ -> e)

zipBack : (Optional Expr -> Expr -> Optional Expr) -> [Optional Expr] -> [Expr] -> [Optional Expr]
zipBack f olds news = match (olds, news) with
  (o +: os, n +: ns) -> f o n +: zipBack f os ns
  _ -> []

-- one step smaller: the node replaced by 0 or by one of its children, a
-- clause dropped, or the same in a child
shrinks : Expr -> [Expr]
shrinks e = match children e with
  (kids, rebuild) ->
    dropped = match e with
      Handle (Handler p cs i) b ->
        go j = if j >= List.size cs then [] else
          match List.at j cs with
            Some (Some _) -> Handle (Handler p (replaceAt j None cs) i) b +: go (j + 1)
            _ -> go (j + 1)
        go 0
      _ -> []
    inner j = cases
      [] -> []
      k +: rest -> lmap (k' -> rebuild (replaceAt j k' kids)) (shrinks k) List.++ inner (j + 1) rest
    (Lit 0 +: kids) List.++ dropped List.++ inner 0 kids

exprSize : Expr -> Nat
exprSize e = match children e with
  (kids, _) ->
    go acc = cases
      [] -> acc
      k +: rest -> go (acc + exprSize k) rest
    go 1 kids

-- the well-typed programs one step smaller, the smallest first
candidates : Expr -> [Expr]
candidates e =
  insert x = cases
    [] -> [x]
    y +: rest -> match (x, y) with
      ((n, _), (m, _)) -> if n <= m then x +: (y +: rest) else y +: insert x rest
  sorted acc = cases
    [] -> acc
    c +: rest ->
      if typeOf [] c == Some progTy then sorted (insert (exprSize c, c) acc) rest
      else sorted acc rest
  lmap (cases (_, c) -> c) (sorted [] (shrinks e))

-- a smaller program that still fails: the biggest cuts are tried first,
-- and at most `fuel` programs are checked
shrink : (Expr ->{e} Boolean) -> Nat -> Expr ->{e} Expr
shrink failing fuel e =
  go fuel e = cases
    [] -> e
    c +: rest ->
      if fuel == 0 then e
      else if failing c then go (Nat.drop fuel 1) c (candidates c)
      else go (Nat.drop fuel 1) e rest
  go fuel e (candidates e)

-- The checks.

roundTrip : a ->{IO} a
roundTrip x =
  match Value.deserialize (Value.serialize (Value.value x)) with
    Left e -> bug e
    Right v -> match Value.load v with
      Left _ -> bug "missing code"
      Right a -> a

-- a result, or None if computing it fails
attempt : '{IO} a ->{IO} Optional a
attempt p = handle IO.tryEval p with cases
  { a } -> Some a
  { Exception.raise _ -> _ } -> None

-- The program with a call of the run's function around some of its
-- arithmetic: the generators make no Call, and this puts calls of a
-- function the specializer doesn't know where the program multiplies or
-- compares, under whatever handlers are there. At most two, the first in
-- the program's text: each call is a request, and in a loop under a
-- handler that resumes twice many of them take the plain run too long.
withCalls : Expr -> Expr
withCalls e =
  go : Nat -> Expr -> (Nat, Expr)
  go left e = match children e with
    (kids, rebuild) ->
      each left acc = cases
        [] -> (left, acc)
        k +: rest -> match go left k with
          (left', k') -> each left' (List.snoc acc k') rest
      match each left [] kids with
        (left', kids') ->
          if left' == 0 then (0, rebuild kids') else
            match rebuild kids' with
              Prim Mul a b -> (Nat.drop left' 1, Call (Prim Add a b))
              Prim Lt a b -> (Nat.drop left' 1, Call (Prim Sub a b))
              e' -> (left', e')
  match go 2 e with (_, e') -> e'

-- The checks, each against f x: specialize f x; specializing the first
-- input too; specialize f x after a round trip through serialization;
-- specializing twice giving equal values; and the program with calls of an
-- unknown function in it ('withCalls'). A specialized run that fails
-- disagrees.
checkN : Nat -> Expr -> Nat -> Nat ->{IO} Boolean
checkN k prog a b =
  expected = Some (run prog a b)
  match k with
    0 -> attempt (do specialize run prog a b) == expected
    1 -> attempt (do specialize (run prog) a b) == expected
    2 -> attempt (do roundTrip (specialize run prog) a b) == expected
    3 -> attempt (do (Universal.==) (specialize run prog) (specialize run prog)) == Some true
    _ ->
      p = withCalls prog
      attempt (do specialize runWith p requesting a b) == attempt (do runWith p requesting a b)

-- the first check that fails, if any
failing : Expr -> Nat -> Nat ->{IO} Optional Nat
failing prog a b =
  go k = if k >= 5 then None else if checkN k prog a b then go (k + 1) else Some k
  go 0

-- what specializing on the program did: gave up (0), kept a handler (1),
-- or fused every handler (2)
classify : Expr -> Nat
classify prog =
  ir = runtime.ir (specialize run prog)
  if ir == runtime.ir (run prog) then 0
  else match Text.indexOf "handle {" ir with
    Some _ -> 1
    None -> 2

genCase : Nat -> (Expr, Nat, Nat)
genCase seed = runRand seed do
  p = genProg 12
  (p, below 8, below 8)

type Report = Report [(Nat, Nat, Expr, Nat, Nat)] Nat Nat Nat

-- cases from seed `from`, `count` of them: the failures, each with its
-- seed, the check that failed, the program shrunk, and the inputs; then how
-- many gave up, kept a handler, fused all
check : Nat -> Nat ->{IO, Exception} Report
check = checkWith genCase

-- the same for loops under handlers
genLoopCase : Nat -> (Expr, Nat, Nat)
genLoopCase seed = runRand seed do
  p = genLoopProg 8
  (p, below 6, below 6)

checkWith : (Nat -> (Expr, Nat, Nat)) -> Nat -> Nat ->{IO, Exception} Report
checkWith genCase from count =
  go i fails gaveUp kept fused =
    if i >= from + count then Report fails gaveUp kept fused else
      match genCase i with
        (prog, a, b) ->
          -- shrunk against the one check that failed
          fails' = match failing prog a b with
            None -> fails
            Some k -> List.snoc fails (i, k, shrink (p -> not (checkN k p a b)) 300 prog, a, b)
          match classify prog with
            0 -> go (i + 1) fails' (gaveUp + 1) kept fused
            1 -> go (i + 1) fails' gaveUp (kept + 1) fused
            _ -> go (i + 1) fails' gaveUp kept (fused + 1)
  go from [] 0 0 0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Bin
  + ability Eff
  + type Env
  + type Expr
  + type Handler
  + ability Rand
  + type Report
  + type Ty
  + type Val

  + allClauses  : [Ty] -> [Optional Expr] -> Boolean
  + apply       : Val -> Val ->{Eff} Val
  + attempt     : '{IO} a ->{IO} Optional a
  + candidates  : Expr -> [Expr]
  + check       : Nat -> Nat ->{IO, Exception} Report
  + checkN      : Nat -> Expr -> Nat -> Nat ->{IO} Boolean
  + checkWith   : (Nat -> (Expr, Nat, Nat))
                  -> Nat
                  -> Nat
                  ->{IO, Exception} Report
  + children    : Expr -> ([Expr], ([Expr] -> Expr))
  + classify    : Expr -> Nat
  + eval        : Env -> Expr ->{Eff} Val
  + exprSize    : Expr -> Nat
  + ext         : Env -> Nat ->{Eff} Nat
  + failing     : Expr -> Nat -> Nat ->{IO} Optional Nat
  + gen         : [Ty] -> Ty -> Nat ->{Rand} Expr
  + genCase     : Nat -> (Expr, Nat, Nat)
  + genClause   : [Ty] -> Nat ->{Rand} Expr
  + genLoopCase : Nat -> (Expr, Nat, Nat)
  + genLoopProg : Nat ->{Rand} Expr
  + genN        : [Ty] -> Nat ->{Rand} Expr
  + genProg     : Nat ->{Rand} Expr
  + genTy       : Nat ->{Rand} Ty
  + handleWith  : Env -> Handler -> '{Eff} Val ->{Eff} Val
  + isN         : [Ty] -> Expr -> Boolean
  + lmap        : (a ->{e} b) -> [a] ->{e} [b]
  + lookup      : Nat -> Env -> Val
  + nat         : Val -> Nat
  + pick        : [a] ->{Rand} Optional a
  + prim        : Bin -> Nat -> Nat -> Nat
  + progTy      : Ty
  + replaceAt   : Nat -> a -> [a] -> [a]
  + requesting  : Nat ->{Eff} Nat
  + roundTrip   : a ->{IO} a
  + run         : Expr -> Nat -> Nat -> Nat
  + runRand     : Nat -> '{Rand} a -> a
  + runWith     : Expr -> (Nat ->{Eff} Nat) -> Nat -> Nat -> Nat
  + shrink      : (Expr ->{e} Boolean) -> Nat -> Expr ->{e} Expr
  + shrinks     : Expr -> [Expr]
  + topH        : Request {Eff} Val -> Val
  + typeOf      : [Ty] -> Expr -> Optional Ty
  + varsOf      : Ty -> [Ty] -> [Nat]
  + withCalls   : Expr -> Expr
  + zipBack     : (Optional Expr
                   ->{g1} Expr
                   ->{g} Optional Expr)
                  -> [Optional Expr]
                  -> [Expr]
                  ->{g, g1} [Optional Expr]

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.
```

Each case is a seed: a program and two inputs. The report lists the cases that disagree,
each with its seed, which check failed, its program shrunk, and the inputs; then how many
cases the specializer gave up on, how many kept a handler, and how many fused every handler.
`checkLoopSeeds` is the same over the second generator, of loops under handlers.

``` unison
checkSome : '{IO, Exception} Report
checkSome = do check 0 50

-- seeds that found bugs
regressions : '{IO, Exception} Report
regressions = do check 100 1

checkLoopSeeds : '{IO, Exception} Report
checkLoopSeeds = do checkWith genLoopCase 0 30

-- Loops under a handler whose case goes on after resuming, or resumes
-- twice, which the generator seldom makes: the loop becomes a function
-- that takes the rest of the body and gives the handle's value.
around : Expr -> Expr -> Expr
around clause body =
  Lam TN (Lam TN (Handle (Handler (Prim Add (Var 0) (Var 1)) [Some clause, None] (Lit 0)) body))

loops : [Expr]
loops =
  [ -- a request each round, counted after the resumption
    around
      (Prim Add (Resume (Var 2) (Var 0) (Prim Add (Var 1) (Lit 1))) (Lit 1))
      (Iter (Var 1) (Lam TN (Perform 0 (Prim Add (Var 0) (Var 2)))) (Var 0)),
    -- the same in a recursion that isn't a tail call
    around
      (Let TN (Resume (Var 2) (Prim Add (Var 0) (Lit 1)) (Var 1)) (Prim Mul (Var 0) (Lit 3)))
      (PrimRec (Var 1) (Lam TN (Lam TN (Prim Add (Perform 0 (Var 1)) (Var 0)))) (Var 0)),
    -- resumed twice, while the state is small
    around
      (If (Prim Lt (Var 1) (Lit 2))
        (Resume (Var 2) (Var 0) (Var 1))
        (Prim Add
          (Resume (Var 2) (Var 0) (Prim Add (Var 1) (Lit 1)))
          (Resume (Var 2) (Lit 1) (Prim Add (Var 1) (Lit 1)))))
      (Iter (Var 1) (Lam TN (Prim Add (Var 0) (Perform 0 (Var 0)))) (Var 0)),
    -- resumed in a function
    around
      (Let (TF TN TN) (Lam TN (Resume (Var 3) (Var 0) (Var 2))) (Prim Add (App (Var 0) (Var 1)) (Lit 2)))
      (PrimRec (Var 0) (Lam TN (Lam TN (Perform 0 (Prim Add (Var 0) (Var 1))))) (Var 1))
  ]

-- each program on every pair of inputs below 4: how many agree, and the
-- ones that don't, with the check that failed
checkLoops : '{IO, Exception} (Nat, [(Nat, Nat, Nat, Nat)])
checkLoops = do
  go i a b ok bad =
    if i >= List.size loops then (ok, bad)
    else if a >= 4 then go (i + 1) 0 0 ok bad
    else if b >= 4 then go i (a + 1) 0 ok bad
    else match List.at i loops with
      None -> (ok, bad)
      Some prog -> match failing prog a b with
        None -> go i a (b + 1) (ok + 1) bad
        Some k -> go i a (b + 1) ok (List.snoc bad (i, k, a, b))
  go 0 0 0 0 []
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + around         : Expr -> Expr -> Expr
  + checkLoops     : '{IO, Exception} ( Nat,
                       [(Nat, Nat, Nat, Nat)])
  + checkLoopSeeds : '{IO, Exception} Report
  + checkSome      : '{IO, Exception} Report
  + loops          : [Expr]
  + regressions    : '{IO, Exception} Report

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Done.

scratch/main> run checkSome

  Report [] 0 0 50

scratch/main> run regressions

  Report [] 0 0 1

scratch/main> run checkLoopSeeds

  Report [] 0 10 20

scratch/main> run checkLoops

  (64, [])
```
