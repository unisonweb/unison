# How Unison's specializer works

This document describes the design of Unison's specializer. The code is [`Specialize.hs`](../Specialize.hs), which has comments covering implementation details.

## The idea

`specialize f x` behaves exactly like `f x`, but unlike ordinary function application, it specializes and recompiles `f` given the argument `x`:

```unison
specialize : (a ->{g} b) -> a ->{g} b
```

Note that this signature works for specializing functions that take any number of arguments.

Let's look at an example:

```unison
power n x = if n == 0 then 1 else x * power (n - 1) x
```

The call `power 3 x` tests `n`, subtracts and recurses three times. But if `n` is known to be 3 ahead of time, none of that needs to happen at runtime. `specialize power 3` produces the code a person would write by hand:

```unison
x -> x * (x * (x * 1))
```

The specialized version of `power` given its first argument, `3`, is produced by *partial evaluation*. A partial evaluator runs a program on partially known input, producing a *residual* program that has been specialized for that input and often runs much faster.

This can be to eliminate the overhead of generic code. For instance, `specialize sortBy Nat.compare` returns a sorting function specialized to `Nat`.

It can also be used for specializing away interpreter overhead for domain-specific languages. For instance, here's a tiny DSL that supports arithmetic and function calls:

```unison
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
  Call i a     -> eval prog (List.at! i prog) (eval prog a x)

run prog = eval prog (Call 0 Arg)
```

And here's the definition of the fibonacci function:

```unison
fibProg : [Expr]
fibProg =
  [IfZero Arg (Num 0)
    (IfZero (Sub Arg (Num 1)) (Num 1)
      (Add (Call 0 (Sub Arg (Num 1))) (Call 0 (Sub Arg (Num 2)))))]

fib = specialize run fibProg
```

`fib` comes out the same as if it were written by hand, and runs at the same speed as hand-written code:

```unison
fibByHand : Nat -> Nat
fibByHand x =
  if x == 0 then 0
  else if x - 1 == 0 then 1
  else fibByHand (x - 1) + fibByHand (x - 2)
```

You can inspect the results of specialization with a new builtin, `runtime.ir : a -> Text` which prints the runtime representation of a Unison value. Here's `runtime.ir (specialize run fibProg)`:

```
eval.specialized#3pt726rv21 prog e x =
  eval' x1 =
    lit = 0
    v1 = x1 == lit
    match v1 with
      false ->
        lit1 = 1
        v2 = Nat.drop x1 lit1
        lit2 = 0
        v3 = v2 == lit2
        match v3 with
          false ->
            lit3 = 1
            v4 = Nat.drop x1 lit3
            v5 = eval' v4
            lit4 = 2
            v6 = Nat.drop x1 lit4
            v7 = eval' v6
            v5 Nat.+ v7
          _ -> 1
      _ -> 0
  eval' x
```

Notice all the `Expr` machinery is gone! `eval'` is `fibByHand`, and the entry just calls it.

More generally, using partial evaluation to specialize an interpreter for a particular program yields compiled code for that program. This incredible fact is the [first Futamura projection](https://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html).

## How it works

`specialize f x` does the following:

1. It evaluates `f x` as normal. If the result is a closure (a partially applied function), move to step 2. If it's an ordinary value like `42`, then we're done.
2. Look up the code for the returned closure. This code is in Unison's intermediate representation, the `ANF` type.
3. Partially evaluate the returned closure, treating `x` as "known". This produces a *residualized* version of the closure, and possibly some residual versions of functions called by `f`.
4. Load the residual `f` and its residual dependencies in one group, and return a Unison `Closure` pointing to it. This group of functions will be run efficiently with the interpreter and the JIT will compile them when hot.

The specializer is a function from a closure to a closure, run inside the runtime, on the same intermediate code the interpreter executes.

### Partial evaluation, a worked example

Step 3 is where the interesting parts are. Here is a walkthrough of what happens given `specialize power 3`:

```unison
power n x = if n == 0 then 1 else x * power (n - 1) x
```

1. **Start.** `n` is known to be 3. `x` is not known, so it becomes a parameter of the code being generated.
2. **`n == 0`.** Both operands are known, so the partial evaluator computes it: `false`.
3. **The `if`.** Its condition is known to be `false`, so only the `else` branch is looked at. The `if` is gone from the output.
4. **`n - 1`.** Computed: `2`.
5. **`power 2 x`.** The function being called is known, so its body is inlined, with `n = 2`. Steps 2 to 5 repeat for 2 and 1.
6. **`n == 0`.** The test is `true` this time, and the `then` branch gives the known value `1`.
7. **`x * 1`, on the way back out.** `x` is not known, so this cannot be computed. The multiplication is emitted, and its result gets a fresh variable `a` in the output. The same happens twice more.

Here's the actual code `runtime.ir (specialize power 3)`:

```
-- specialize power 3
power.specialized#tlbm35jj8q n x =
  lit = 1
  v1 = x Nat.* lit
  v2 = x Nat.* v1
  x Nat.* v2
```

This example illustrates the basic idea that **decisions made on known values vanish during partial evaluation;** only operations on unknown values end up emitted in the residual function.

Now a few observations:

- This example was overly simple in that **every variable was either completely known or completely unknown** (`n` was fully known, `x` was fully unknown). We'll want a middle ground, such as a pair whose first element is known, or an `Either` which is known to be `Left`, but with an unknown value inside the `Left`.
- **The recursion ended because `n` was known.** In other code, recursion may be on unknown values, and inlining would never stop. Recognizing this and turning it into a loop in the output is the subject of "Recursion and termination".

Let's discuss these in turn.

## Abstract values: what is known about each variable

An ordinary evaluator maps each variable to a value. The specializer maps each variable to what it knows about the value, which may be everything, nothing, or something in between.

```haskell
data Abs
  = Known Val (Maybe Symbol)   -- known entirely: a runtime value
  | Con Reference CTag [Abs]   -- a known constructor, fields partially known
  | Clo Fn [Abs]               -- a known function applied to partially known arguments
  | Dyn Symbol Mem             -- unknown: a variable of the residual program
  | SReq Reference CTag [Abs] Int  -- a request being handled while specializing
  | SPure Abs                      -- the value a fused handler's body finished with
  | SKont Int                      -- the continuation of such a request
  | SBridge Symbol Int             -- a request that isn't known (see the bridge)
```

Let's look at the first four, recalling our simple `power` example:

```unison
power n x = if n == 0 then 1 else x * power (n - 1) x
```

- **`Known`** holds a real runtime value: a number, a list, a closure. `specialize power` `3` produces a `Known` for power's first parameter, `n`.
- **`Dyn`** names a variable in the code being generated. It is completely unknown. In `specialize power 3`, the second parameter to `power`, `x`, will be `Dyn`. The result of a call written `specialize.boundary f x` is also `Dyn`: the specializer keeps that call as a call.
- **`Con` and `Clo`** are the in-between cases, not used by `specialize power 3`, but they are what makes the partial evaluator more than constant folding. A pair of a known tag and an unknown number is a `Con` whose match can still be decided now. A lambda that captures an unknown variable is a `Clo` that can still be inlined at its call.

The last four exist for specializing ability handlers and are covered later.

## The partial evaluator

The partial evaluator is one recursive function, `partialEval`, over the runtime's A-normal form (ANF) representation of code. It reads like an interpreter written in continuation passing style, and it produces a rewritten ANF program.

```haskell
type Term = ANormal Reference Symbol

partialEval :: SpecEnv -> SpecLimits -> E -> Term -> K -> M Term
partialEval env lim e t k =
  tick lim >> case t of
    TVar v -> ret k (look e v)
    TLets _ [v] [m] bn bo ->
      partialEval env lim e bn . KBind m v $ \a -> partialEval env lim (bind v a e) bo k
    TMatch v br -> match env lim e v br k
    TCon r tg as -> ret k (Con r tg (map (look e) as))
    TApp (FVar f) as -> call env lim e (look e f) (map (look e) as) k
    TApp (FPrim p) as -> prim env e p (map (look e) as) k
    TApp (FReq r t) as -> request env lim e r t (map (look e) as) k
    THnd rs h Nothing body -> handler env lim e t rs h body k
    ...
    _ -> opaque env e t k
```

- **`E`** is the environment: each variable's `Abs`.
- **`K`** is the continuation, where the partially evaluated result (of type `Abs`) should be passed.

  ```haskell
  data K
    = KBind Mem Symbol (Abs -> M Term)
        -- bind the result to a variable (a name hint and its
        -- representation), then run the rest of the walk
    | KTail           -- nothing follows: the value is the code's result
    | KDelim Int      -- the end of a fused handler's body
    | KReturn Int     -- the end of a state-passing residual function
    | KAbort Int Int  -- the value of a handler case that didn't resume, in such a function
    | KEnd Int        -- the end of a value-returning residual function
    | KOuter Int Mem  -- what follows a fused `handle`, by name
  ```
- **`M`** is a state monad over `IO`: fresh names, the functions generated so far, budgets, and the handlers in force.
- **The result**, the `M Term`, is not the residual code for that value: it is the residual code for this term *and everything after it*.

### Matches on known values disappear; matches on unknown values refine what is known in each arm

A match on something known picks its arm now, and the match disappears:

```haskell
match env lim e v br k = case (look e v, br) of
  (Con _ t fs, MatchData _ cs df)
    | Just (_, ABTN.TAbss vs body) <- EC.lookup t cs ->
        partialEval env lim (foldr (uncurry bind) e (zip vs fs)) body k
  ...
  _ -> dynamic
```

This is also the case that deletes an interpreter's dispatch overhead, like in our `Expr` example: `match e with Add a b -> ...` on a known `e` becomes just the `Add` arm.

A match on something unknown (`dynamic`) stays in the output, refining what is known in each arm. For instance, in:

```unison
match x with
  Left a -> ...
```

Within the `Left a` branch, `x` is known to be `Left a` for the newly generated (and unknown) variable `a`.

### Primitives and calls

- **A primitive with all arguments known** is computed (`3 + 4` becomes `7`). Otherwise it is emitted. Impure primitives are always emitted.
- **A call of a known function** is where the interesting policy lives, covered next.
- **A call of an unknown function** stays as a call, with its arguments materialized by `Unison.Runtime.ANF.Specialize.toVar`.
- **Anything `partialEval` has no case for** goes to `opaque`. `opaque` runs the expression if all its free variables are known, otherwise it copies the expression into the output unchanged. This ensures the partial evaluator stays correct on constructs it does not model.

### Why the partial evaluator is in continuation-passing style

Let's look at a simple program fragment:

```unison
v = foo x y
bar v
```

Whether the output needs a `let` here depends on what `foo x y` turns out to be. It may be that `foo x y` is fully known and can be simplified away and inlined into `bar`. Or it may be that some of the call to `foo` remains. CPS gives the partial evaluator the flexibility to do both. It can either eliminate a `let`, or it can keep it in the output.

## Recursion and termination

A call of a known function can be handled two ways:

- **Unfold:** The callee is inlined into the caller. The caller then knows what the call produced and can keep specializing on it.
- **Residualize:** emit a call to a *residual function*, a new function whose body is the callee's, specialized on what is known of the arguments. The result of the call is unknown to the caller.

`power 3 x` unfolds three times and stops, because `n` is known and reaches 0. But `fac x`, where `fac n = if n == 0 then 1 else n * fac (n - 1)`, recurses on an unknown `x`: unfolding it would never stop. Residualizing is what turns that recursion into a loop in the output.

### The rule for deciding when to unfold or residualize

The main idea is to unfold whenever it is safe, and residualize only to stop a walk that might not end.

Unfolding is safe as long as every branch taken so far was decided by a known value. If we are taking branches based on a known value, we are doing exactly what the program would do at run time, and it will terminate whenever the program does. `power 3 x` unfolds three times because the program itself recurses three times.

Under a match on an unknown value, that guarantee is gone. Each arm of the match is code that may never be hit at runtime. The walker has to explore every arm, including paths that may never run, so a recursive call there could unfold forever. In `fac x`, the test `n == 0` is unknown, so the walker explores the `else` arm, meets `fac (n - 1)`, explores its `else` arm, and so on.

So under an unknown match, the walker unfolds a call only if it doesn't look like a loop. A call looks like a loop in either of two cases:

- **It repeats a call already in progress.** The walker is still in the middle of unfolding the same function, possibly several calls up, and the known part of the arguments hasn't gotten smaller (it isn't a sub-expression or a smaller literal of what was known before).
- **It has been seen before.** A residual function for the same arguments already exists.

Take `fac x`. The walker unfolds it, hits the unknown `n == 0` test, explores the `else` arm, and meets `fac (n - 1)`. It is still unfolding `fac`, and the argument `n - 1` is unknown just as `x` was, so nothing known got smaller. That looks like a loop, so instead of unfolding, the walker emits a call to a residual function. That call is the loop in the output.

When the call in progress that it repeats has the very same key, that unfolding was the first round of the loop. Leaving it in place would put the loop's body in the output twice, once inline and once as the residual function. So the walker goes back to that call, discards its unfolding, and makes it a call to the residual function as well (`Redo`, caught by `unfolding`). The call is remembered as the start of a loop, so it is residualized at once if it is met again.

Separately, there is a depth limit of 200 nested unfoldings. Past it, a call is residualized even with no unknown match above it. This caps inlining in cases the loop check doesn't catch, for example a program that recurses very deeply on known data. It is not a termination guarantee by itself; for that we use the whistle and the hard budgets described later.

In the code, it is one test, in `call` in `Unison.Runtime.ANF.Specialize` (names simplified here):

```haskell
if depth < limit && not (underUnknownMatch && looksLikeLoop)
  then unfold
  else residualize
```

### Memoization: one function per key

A residual function is identified by a key: the function and what is known of each argument, with every unknown part a hole.

```haskell
data Key = Key Int [Reference] Fn [KArg]

-- one argument's part of a key: its Abs, with unknown parts as holes
data KArg
  = KKnown Val                     -- a known value
  | KCon Reference CTag [KArg]     -- a known constructor
  | KClo Fn [KArg]                 -- a known closure
  | KHole Mem                      -- unknown: becomes a parameter
  | ...

keyOf :: Abs -> KArg
keyOf = \case
  Known v _  -> KKnown v
  Con r t fs -> KCon r t (map keyOf fs)
  Clo f as   -> KClo f (map keyOf as)
  Dyn _ m    -> KHole m
  ...
```

The key goes into a memo table *before* the body is walked. So when the body calls the same function with the same key, it finds the entry and emits a call to the function being built. That is how recursion in the source becomes recursion in the output, and why `eval prog <body of 0> <unknown>` met a second time closes the loop.

The holes become the residual function's parameters. `eval prog body x` with `prog` and `body` known and `x` unknown becomes a one-parameter function.

### The whistle

Memoization only helps if keys repeat. Some programs make the known part grow on every round. (For instance, an interpreter in continuation-passing style is this case: each recursive call passes a bigger known continuation, so every key is new and the partial evaluator would generate functions forever.)

The partial evaluator detects this with a standard test from the supercompilation literature, homeomorphic embedding ([Sørensen and Glück 1995](https://bibtex.github.io/ILPS-1995-SorensenG.html); [Leuschel 1998](https://link.springer.com/chapter/10.1007/3-540-49727-7_14)). The function `Unison.Runtime.ANF.Specialize.embeds a b` returns true if (roughly) `b` is `a` with parts added, so `a` is the earlier, smaller key.

```haskell
embeds :: KArg -> KArg -> Bool
embeds a b = couple a b || dive b
  where
    couple (KHole _) _ = True
    couple (KKnown v) (KKnown w) = knownLE v w
    couple (KCon r t xs) (KCon r' t' ys) = r == r' && t == t' && and (zipWith embeds xs ys)
    ...
    dive = \case
      KCon _ _ ys -> any (embeds a) ys   -- a is inside one of b's parts
      ...
```

Before building a residual function for a new key, the partial evaluator checks it against the keys of residual functions for the same function that are still being built further up the walk. If `embeds old new` holds for one of them, the new key is just an old one with parts added: it is growing. In the supercompilation literature, this is called "blowing the whistle".

When the whistle is blown, the partial evaluator then generalizes the new key (`Unison.Runtime.ANF.Specialize.generalize`): the parts where it differs from the old key become holes, so they turn into parameters passed at run time. The key stops growing, so it repeats, and the memo table ensures subsequent calls get the same treatment. Only the parts that kept growing are no longer specialized on, and everything the two keys share stays known.

Two known literals couple only if the old one is no larger than the new one (`knownLE`). So a known counter going down, such as 5 then 4, never looks like growth, and a loop over a known count unfolds fully. A counter going up does count as growth, so the whistle can blow on it. `knownLE` orders a `Nat` by value, an `Int` by absolute value and a `Text` by size. Any two `Float`s couple, and anything else couples only when equal.

### Budgets

The whistle is a heuristic, so hard limits back it up: steps of the partial evaluation walk (200,000), residual functions (500), unfolding depth (200), and the size of the generated code, set to a multiple of the size of the input function and its known values. Hitting the step or function limit means giving up, and returning the original closure unchanged, which is always correct. Hitting the depth limit residualizes the call instead, and hitting the size limit first retries with no handler fused (see "Failing safely").

## What runs at specialization time

When everything a piece of code needs is known, the specializer does not walk it. It runs it on the real interpreter.

```haskell
-- A term whose free variables are all known, run on the interpreter.
runIfKnown :: SpecEnv -> E -> Term -> M (Maybe Val)
runIfKnown env e t = do
  let fvs = S.toList (ABTN.freeVars t)
      abss = map (look e) fvs
  allKnown env abss >>= \case
    Nothing -> pure Nothing
    Just vs -> liftIO (seRunTerm env (zip fvs (map memOf abss)) t vs)
```

The same applies to a call whose function and arguments are all known. This matters for two reasons:

- **Coverage.** The walker models a subset of the language precisely. The interpreter implements all of it, including every builtin and every kind of handler. Known code that the walker has no case for still gets evaluated.
- **Speed.** Computing `List.at 3 prog` on known values by running the builtin is far cheaper than symbolically walking a library function.

Running code during specialization must not move, repeat or drop the program's effects, so it is guarded:

- **Only pure code runs.** A run is refused if it would perform an effect or touch a mutable value (a `Ref`, an `MVar`, a handle). Those are treated as known but opaque: they can be passed around and stored, and every operation on them stays in the output.
- **Runs are bounded in time.** A known computation that would not finish is cut off, and the code stays in the output for run time to deal with.
- **A failing run is not an error.** If known code would crash (`List.at!` out of range under a branch that never executes), the specializer emits the code instead. The crash happens at run time, if that path is ever taken, as it would have without `specialize`.

## Abilities: removing handlers

`handle` blocks and ability requests get specialized as well. It's about half of the specializer's code.

The running example for this section is a state handler, `runSt`, and a loop that uses it, `count`. `count n` adds `n`, `n - 1`, …, `1` to the state and returns the final state:

```unison
ability St s where
  get : {St s} s
  put : s ->{St s} ()

runSt : s -> '{g, St s} a ->{g} a
runSt s p =
  go s = cases
    { a } -> a
    { St.get -> k } -> handle k s with go s
    { St.put s' -> k } -> handle k () with go s'
  handle p () with go s

count : Nat ->{St Nat} Nat
count n =
  if n == 0 then St.get
  else
    St.put (St.get + n)
    count (n - 1)
```

So `runSt 2 do count 4` is 2 + 4 + 3 + 2 + 1 = 12.

There are three levels, tried in order.

### 1. Everything known: it runs

A `handle` block whose free variables are all known is run on the interpreter, like any other known code. `runSt 2 do count 4` becomes `12`.

### 2. The handler stays, its body is specialized

Otherwise the `handle` is kept in the output. The code under it is specialized as usual and its requests stay requests. An interpreter that uses an ability only for errors already loses all of its interpretive overhead this way.

### 3. Fusing the handler away

If the handler is a known function, the specializer tries to remove the `handle` altogether. No handler is emitted, and when the walk reaches a request, it handles it on the spot:

- The handler's function is unfolded on a *static request* (`SReq`): the ability, the operation and its arguments.
- The handler's match on the request picks its case now, as a known match.
- The case's continuation variable is bound to a *static continuation* (`SKont`). This is the rest of the walk from the request, which the walker has in hand as its own `K`, the reason `partialEval` is in continuation-passing style.
- Resuming it just carries on the walk with the resumed value.

Take a state handler and a straight-line body:

```unison
runSt s p =
  go s = cases
    { a } -> a
    { St.get -> k } -> handle k s with go s
    { St.put s' -> k } -> handle k () with go s'
  handle p () with go s
```

Walking `St.put (St.get + n)` under it: `St.get` resumes with the state, so it *is* the state. `St.put v` resumes under a handler holding `v`, so the walker's record of the handler's state becomes `v`. No request, no continuation capture and no handler closure appear in the output. The state has become an ordinary variable.

An abort handler becomes a branch. This is the full residual code for an interpreter with an `Abort` ability, specialized on the program `x * x - 10`, where subtraction below zero aborts:

```
prog x =
  va = x Nat.* x
  lit = 10
  v1 = Nat.lt va lit
  match v1 with
    false ->
      r = Nat.drop va 10
      Some r
    _ -> None
```

The walker keeps the handlers in force as a stack of frames in its state, and for each one where its body's value goes. A case that does not resume drops the rest of the body; `handle k v with h'` carries on under `h'`; a plain `k v` returns to the case, which can do more work after.

### Loops under a fused handler: passing the state

A recursive function under a fused handler cannot be unfolded forever, and a residual function cannot make requests to a handler that no longer exists. So the residual function is given the handler's state as an extra parameter and returns it:

- **Each fused handler in force is one more argument.**
- **At its end the function returns `Right (result, state)`.**
- **A case that does not resume returns `Left value`,** the value of the whole `handle`.
- **The call site matches on the result** and carries on fused, with the state it got back.

So `runSt s do count n`, with `count` a loop that reads and writes the state each round, becomes a two-parameter loop over `n` and the state, running as fast as the same loop written by hand without abilities.

State passing needs every resuming case to re-install the handler as its last act. When a case does more after resuming, or resumes twice (a nondeterminism handler), the loop instead takes the rest of the caller's body as a function parameter and returns the `handle`'s value.

### Join points

Under a fused handler, the code after an unknown match cannot simply follow the match in the output, because a request inside an arm must see all of its continuation. Copying the rest of the walk into every arm is correct but doubles the code at each `if`. Instead the rest becomes a *join point*: a small residual function of the values the arms produce and the handler states they end with, called at the end of each arm. The arms are walked first, what they produce is generalized together to find the parameters, and the rest is walked once.

### The bridge: unknown code under a fused handler

A call of an unknown function is the hard case: its code is not walked, so its requests are real, and there is no real handler to catch them. The call is made under one, built from the fused handler's own cases:

```haskell
bridgeCall env lim code k = do
  fs <- gets (takeWhile isFused . sFrames)
  (hf, tmpls, leaves) <- bridgeFn env lim False fs
  toVars (concat leaves) \xs -> do
    h <- freshNamed (syn "bridge")
    r <- freshNamed (syn "r")
    rest <- resultMatch fs tmpls r k
    pure (TName h (Right hf) xs (TLets (Indirect n) [r] [BX] (THnd (concatMap frRefs fs) h Nothing code) rest))
```

The bridge takes the handler's state and gives it back in the same `Right (result, state)` or `Left value` form a state-passing loop uses, so the call site continues the same way. For `runSt s do St.put (f St.get); St.get` with `f` unknown, the output is:

```
s f =
  bridge' general request =
    match request with
      {St.get} ->
        cont1 = shift {St}
        bridge1 = partialApply bridge' general
        handle {St} cont1 general with bridge1
      {put s'} ->
        cont2 = shift {St}
        bridge2 = partialApply bridge' s'
        handle {St} cont2 () with bridge2
      {r2} -> Right (r2, general)
  bridge = partialApply bridge' s
  r = handle {St} f s with bridge
  match r with
    Right (r1, s1) -> r1
    Left done -> done
```

The `St.get` before the call became `s`. The `St.put` and the final `St.get` vanished: the result is `r1`, which is both what `f` returned and the state after the put.

## Failing safely

Fusing a handler is a bet that the walker will see every request made in the handler's scope. When it finds it cannot, it backs out. There are three levels of retreat, each smaller in scope than the next, and all of them produce correct code.

```haskell
data Escape = Escape Int String   -- back to the fused handler with this number
data GiveUp = GiveUp String       -- abandon this specialization
```

- **`Escape`: keep one handler.** Something in a fused handler's scope could make its requests unseen: a closure escaping as a value, a builtin that calls back into Unison code, a case shape the bridge does not cover. The walker throws `Escape` to that handler's `handle`. The walk's state is rolled back to that point and the `handle` is redone as a kept handler (level 2 above). Everything outside it is unaffected, and other handlers stay fused.
- **Retry with no fusion.** If the generated code exceeds its size budget with handlers fused, the whole specialization is redone once with every handler kept. Fused handlers are what copy code.
- **`GiveUp`: return the original.** If a budget runs out anyway, `specialize f x` returns `f x` unchanged.

The fused attempt, with its rollback:

```haskell
liftIO (try (runStateT attempt s0)) >>= \case
  Right (code, s1) -> put s1 >> pure code
  Left (Escape i why)
    | i == fid -> do
        put s0                                   -- roll back
        liftIO (seNote env ("a handler kept: " ++ why))
        kept                                     -- redo with the handler kept
    | otherwise -> liftIO (throwIO (Escape i why))  -- not ours: pass it outward
```

The walker's state is an immutable value threaded through a state monad, so rolling back is just keeping the old one. This pattern, try the precise thing, catch a typed exception, restore the state and do the conservative thing, appears throughout the file. It keeps each optimization's preconditions local: the code that discovers a problem does not need to know how to recover from it.

`UNISON_SPECIALIZE_DEBUG=1` prints one line per handler kept, with the reason, and the residual code of every specialization.

## A specialized closure as a value

`specialize f x` means the same as `f x`, but it may be a different value: it can compare unequal
to `f x` and hash differently. Its identity is "`f` specialized on `x`".

- **It is an ordinary closure** over a generated function, so equality, ordering and hashing need
  no special case. The generated function's reference is a hash of the closure it was made from,
  not of the generated code. Two specializations of the same `f` on the same `x` are equal, in
  this process and in any other, and stay equal when the specializer changes. The second one
  generates nothing.
- **Serialization ships the original closure**, marked as specialized, because no codebase holds
  the generated code. Reading it back specializes it again, which gives the same identity. The
  mark needs version 5 of the value format, which `Value.serialize` uses only for a value that
  holds a specialized closure.
- **Mutable values are known but opaque.** A `Ref` or `MVar` in `x` is passed along and shared
  with `f x`, and every operation on it stays in the output. Closures that differ only in which
  mutable value they hold share the generated code.
- **When `f x` isn't a closure**, including when it has run to a plain value, it is returned as
  it is.

See `specializeClosure` and `specializedRef` in [`Machine.hs`](../../Machine.hs).

## Steering and inspecting

- **`specialize.boundary f x`** means `f x`, and under `specialize` the call stays a call. `f` is
  not inlined, copied or run, even when everything is known, and the result is unknown from then
  on. Use it for slow paths not worth copying into every specialization, for code that should be
  compiled once and shared, and to stop precomputation: `specialize.boundary (n -> List.range 0
  n) 1000000` stays a call instead of a million-element constant. Outside `specialize` it costs
  nothing, since the ordinary optimizer inlines it away.
- **`runtime.ir v`** shows what the runtime runs for a value: for a closure, its captured values
  and its optimized code, with every generated function. Generated code keeps the program's
  variable names, and a residual function is named after the function it came from with a prime
  (`eval'`). The specialized function itself is named `f.specialized#hash`, after the function
  `f` it is a specialization of. The comment above the code is the value as a term; a recursive
  function in it is shown by name, not as a lambda. The text depends on the runtime version, so it is for debugging and transcripts, not
  program logic.
- **`UNISON_SPECIALIZE_DEBUG=1`** prints each specialization's residual code, its constants, why
  it gave up if it did, and one line per handler kept. `UNISON_SPECIALIZE_JOIN=off` and
  `UNISON_SPECIALIZE_BRIDGE=off` turn those two mechanisms off, to compare.

Partial evaluation is only as good as the separation of known from unknown in the code being
specialized:

- Keep the program and the run-time data in separate arguments, or in a structure whose shape is
  known (an environment of known names and unknown values). Don't put them together in one opaque
  value.
- Keep the program in immutable data. Reading a `Ref` that holds the program gives an unknown.
- A match on a value that is unknown stays a match. A lookup by a known key in a structure with
  unknown values resolves while specializing.

## Implementation notes

- **Where the code comes from.** The runtime keeps every loaded function's A-normal form, before
  optimization, keyed by reference (`intermed` in `CCache`). A closure names its function and
  holds its captured values. That is all the specializer reads. `specialize` is a primitive
  (`SPEC`) because it needs that cache.
- **Where the code goes.** The residual functions are loaded as one group with `cacheAdd0`, like
  code loaded at run time, and go through the normal pipeline: the optimizer, code emission and
  the JIT, which is asked for the group at once, with no wait for it to get hot. A specialized
  function can be specialized again.
- **Constants.** A-normal form has no node for an arbitrary value, only literals. A known value
  that isn't a literal becomes a parameter of every residual function that needs it
  (`threadConsts`), and the specialized closure holds the values after its own arguments.
- **Cleaning up.** The walk makes a residual function at every call that looks like a loop and a
  join point at every match under a fused handler. A pass after it (`inlineFns`) inlines the
  ones that aren't recursive and are called once or are small.
- **Running known code.** `runKnown` in `Machine.hs` calls the interpreter on a fresh stack. It
  refuses code that can reach an IO builtin or an operation on mutable state, and bounds the run
  with fuel counted per call and a time limit.
## Testing

The rule that `specialize f x` behaves as `f x` is directly testable, and the main test does exactly that on generated programs.

- **Hand-written cases** live in the transcript `specialize.md`. Each shows a program, its result, and the residual code via the `runtime.ir` builtin, so a change in generated code shows up as a diff in review.
- **The differential tester** (`specialize-differential.md`) contains an interpreter for a small language with functions, bounded loops and effect handlers. It generates random well-typed programs, specializes the interpreter on each, and compares against the plain run. A known program gives residual code in that program's shape, so random programs give varied residual code.
- **Five checks per program:** `specialize f x`; specializing on the first input too; the specialized closure after a round trip through serialization; specializing twice gives equal closures; and the program with calls of an unknown, request-making function inserted.
- **Two generators.** One makes general programs. The other makes loops under one or two handlers whose cases use their continuation in every way: not at all, as the last act, with work after, twice, and inside a function.
- **Failures are shrunk** to a smaller program of the same type that still fails, and reported with the seed.
- **The report counts** how many programs had every handler fused, a handler kept, or were given up on. A run where most cases give up tests little.

## Limits and open questions

What the specializer does not do today, each of which falls back to correct but less specialized code:

- **Handlers that keep the handler.** A case that goes on after resuming, or resumes twice, has no bridge. An unknown call under two or more fused handlers keeps them. A closure that escapes as a value under a fused handler keeps it.
- **Loops under two fused handlers** keep the inner handler when one of its cases has work left after resuming or does not resume.
- **A residual function's result is unknown to its caller.** If a loop always returns a pair, the caller still matches on it at run time. A worker/wrapper split, where the function returns only the unknown parts and the caller rebuilds the known shape, is prototyped on a branch. It was correct on all 1,200 test programs but showed no measurable gain yet, so it is not merged.
- **The whistle's cost.** The embedding test keeps its answer for each pair of subterms, so it takes time proportional to size(a) × size(b). If that ever matters, a tag-bag whistle compares multisets of source-location tags instead of trees (Mitchell 2010; Bolingbroke and Peyton Jones 2010). Tag-bags are cheaper but coarser, so they may generalize sooner.
- **A loop inside a loop is walked once per loop around it.** Going back to the start of a loop discards what was generated since, including any loop finished inside it, which is then generated again. The steps taken still count against the budget.
- **Mutable values are opaque.** A known `Ref` is passed along, and every read stays in the output.

## A map of the code

Almost everything is in one file, `unison-runtime/src/Unison/Runtime/ANF/Specialize.hs` (about 2,700 lines). It has no dependency on the machine: everything it needs from the runtime comes in through a record of callbacks, `SpecEnv`, which makes it testable on its own. The snippets above are trimmed for reading; names match the code.

| To understand | Read |
| --- | --- |
| The entry point and how the output group is assembled | `specializeLambda`, `specializeLambda1` |
| What the runtime provides (code lookup, running known code, notes) | `SpecEnv`, and `specEnv` in `Machine.hs` |
| Abstract values | `Abs`, `toVar`, `keyOf` |
| The walk | `partialEval`, `ret`, `dynResult`, the `K` type |
| Matches, known and unknown | `match`, its local `dynamic` and `joined` |
| Unfold or residualize | `call` (the local `callFn`), `unfolding`, `residualCall` |
| Residual functions and the memo table | `residualFnAs`, `Key` |
| The whistle | `embeds`, `generalize` |
| Running known code | `runIfKnown`, `opaque`; `runTerm` in `Machine.hs` |
| Handlers: kept or fused | `handler` (its local `kept` and `fused`) |
| Requests and static continuations | `request`, `callHandler`, `resume`, `resumeWalk` |
| Loops passing handler state | `residualCall`, `resultMatch`, `returnRight`, `valueCall` |
| The bridge | `bridgeCall`, `bridgeFn`, `resumeBridge` |
| Backing out | `Escape`, `GiveUp`, `guardFused`, `withBarrier` |
| Budgets | `SpecLimits`, `tick`, `emitted`; `specLimits` in `Machine.hs` |
| The builtin, closure identity and serialization | `specializeClosure`, `specializedRef` in `Machine.hs` |

The transcripts `unison-src/transcripts/idempotent/specialize.md` and `specialize-differential.md` are the best place to see real input and output side by side.
