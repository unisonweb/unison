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

This can be used to eliminate the overhead of generic code. For instance, `specialize sortBy Nat.compare` returns a sorting function specialized to `Nat`.

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

`fib` comes out the same as if it were written by hand, and runs at the same speed as hand-written code like this:

```unison
fibByHand : Nat -> Nat
fibByHand x =
  if x == 0 then 0
  else if x - 1 == 0 then 1
  else fibByHand (x - 1) + fibByHand (x - 2)
```

You can inspect the results of specialization with a new builtin, `runtime.ir : a -> Text` which prints the runtime representation of a Unison value. Here's `runtime.ir (specialize run fibProg)`:

```
eval.specialized#02m1laefmu prog e x =
  eval' x1 =
    lit = 0
    v1 = x1 == lit
    match v1 with
      false ->
        lit1 = 1
        v2 = x1 Nat.- lit1
        lit2 = 0
        v3 = v2 == lit2
        match v3 with
          false ->
            lit3 = 1
            v4 = x1 Nat.- lit3
            v5 = eval' v4
            lit4 = 2
            v6 = x1 Nat.- lit4
            v7 = eval' v6
            v5 Nat.+ v7
          _ -> 1
      _ -> 0
  eval' x
```

Notice all the `Expr` machinery is gone! `eval'` is essentially `fibByHand`, and the entry just calls it.

Using partial evaluation to specialize an interpreter for a particular program yields _compiled code for that program._ This incredible fact is the [first Futamura projection](https://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html).

## How it works

Here are the steps involved in `specialize f x`:

1. Evaluate `f x` as normal. If the result is a closure (a partially applied function), move to step 2. Otherwise, stop. 
2. Look up the code for the returned closure. This code is in Unison's intermediate representation, the `ANF` type.
3. Partially evaluate the returned closure, treating `x` as "known". This produces what is called a *residual* version of the closure, and possibly some residual versions of functions called by `f`.
4. Load the residual `f` and its residual dependencies in one group, and return a Unison `Closure` pointing to it. This group of functions will be run efficiently with the interpreter and the JIT will compile them when hot.

As an example, here is a walkthrough of the partial evaluation that happens when calling `specialize power 3`:

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
power.specialized#013qo80bla n x =
  lit = 1
  v1 = x Nat.* lit
  v2 = x Nat.* v1
  x Nat.* v2
```

This example illustrates the basic idea that **decisions made on known values vanish during partial evaluation;** only operations on unknown values end up emitted in the residual function.

Now a few observations:

- This example was overly simple in that **every variable was either completely known or completely unknown** (`n` was fully known, `x` was fully unknown). We'll want a middle ground, such as a pair whose first element is known, or an `Either` which is known to be `Left`, but with an unknown value inside the `Left`.
- **The recursion ended because `n` was known.** In other code, recursion may be on unknown values, and inlining would never stop. Recognizing this and turning it into a loop in the output is the subject of [Recursion and termination](#recursion-and-termination).

Let's discuss these in turn.

## Abstract values: what is known about each variable

An ordinary evaluator maps each variable to a value. The partial evaluator maps each variable to what it knows about the value, which may be everything, nothing, or something in between.

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

The last four (`SReq`, `SPure`, `SKont`, and `SBridge`) exist for specializing ability handlers and are covered later.

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

A match on something known picks its arm in partialEval, and the match disappears:

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

Whether the output needs a `let` here depends on what `foo x y` turns out to be. It may be that `foo x y` is fully known and can be simplified away and inlined into `bar`. Or it may be that some of the call to `foo` remains. CPS gives the partial evaluator the flexibility to do either one.

## Recursion and termination

A call of a known function can be handled two ways:

- **Unfold:** The callee is inlined into the caller. The caller then knows what the call produced and can keep specializing on it.
- **Residualize:** emit a call to a *residual function*, a new function whose body is the callee's, specialized on what is known of the arguments. The result of the call is unknown to the caller.

`power 3 x` unfolds three times and stops, because `n` is known and reaches 0. But `fac x`, where `fac n = if n == 0 then 1 else n * fac (n - 1)`, recurses on an unknown `x`: unfolding it would never stop. Residualizing is what turns that recursion into a loop in the output.

### The rule for deciding when to unfold or residualize

The main idea is to unfold whenever it is safe, and residualize only to stop a walk that might not end.

Unfolding is safe as long as every branch taken so far was decided by a known value. If partial evaluation is taking branches based on a known value, it is doing exactly what the program would do at run time, and it will terminate whenever the program does. `power 3 x` unfolds three times because the program itself recurses three times.

Under a match on an unknown value, that guarantee is gone. Each arm of the match is code that may never be hit at runtime. The evaluator has to explore every arm, including paths that may never run, so a recursive call there could unfold forever. In `fac x`, the test `n == 0` is unknown, so the walker explores the `else` arm, meets `fac (n - 1)`, explores its `else` arm, and so on.

So under an unknown match, the walker unfolds a call only if it doesn't look like a loop. A call looks like a loop in either of two cases:

- **It repeats a call already in progress.** The walker is still in the middle of unfolding the same function, possibly several calls up, and the known part of the arguments hasn't gotten smaller (it isn't a sub-expression or a smaller literal of what was known before).
- **It has been seen before.** A residual function for the same arguments already exists.

Take `fac x`. The walker unfolds it, hits the unknown `n == 0` test, explores the `else` arm, and meets `fac (n - 1)`. It is still unfolding `fac`, and the argument `n - 1` is unknown just as `x` was, so nothing known got smaller. That looks like a loop, so instead of unfolding, the walker emits a call to a residual function. That call is the loop in the output.

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

The key goes into a memo table *before* the body is walked. So when the body calls the same function with the same key, it finds the entry and emits a call to the function being built. That is how recursion in the source becomes recursion in the output.

The holes become the residual function's parameters. `eval prog body x` with `prog` and `body` known and `x` unknown becomes a one-parameter function.

### The whistle

Memoization only helps if keys repeat. Some programs make the known part grow on every recursive call. The partial evaluator detects this with a standard test from the supercompilation literature. The function `Unison.Runtime.ANF.Specialize.embeds a b` returns true if (roughly) `b` is `a` with parts added, so `a` is the earlier, smaller key.

```haskell
embeds :: KArg -> KArg -> Bool
```

There are two ways for `embeds a b` to hold:

- **Coupling:** `a` and `b` are the same at the top, and each part of `a` embeds in the matching part of `b`. "The same at the top" means the same constructor, or a closure of the same function. A hole in `a` couples with anything.
- **Diving:** `a` embeds in one of `b`'s parts. So `b` has something wrapped around `a`.

For example, `Cons 1 _` embeds in `Cons 1 (Cons 2 _)` by coupling: both are a `Cons`, `1` embeds in `1`, and the hole embeds in `Cons 2 _`. And `Cons 2 _` embeds in `Cons 1 (Cons 2 _)` by diving into the tail.

Before building a residual function for a new key, the partial evaluator checks it against the keys of residual functions for the same function that are still being built further up the walk. If `embeds old new` holds for one of them, the new key is just an old one with parts added, implying it is growing.

(In the supercompilation literature, this check is called the _whistle_, as in "blowing the whistle".)

When the whistle is blown, the partial evaluator then generalizes the new key using (`Unison.Runtime.ANF.Specialize.generalize`): the parts where it differs from the old key become holes, so they turn into parameters passed at run time. Thus, the key stops growing, so it repeats, and the memo table ensures subsequent calls get the same treatment. Only the parts that kept growing are no longer specialized on, and everything the two keys share stays known.

One note on coupling: two known literals couple only if the old one is no larger than the new one (`knownLE`). So a known counter going down, such as 5 then 4, never looks like growth, and a loop over a known count unfolds fully. A counter going up does count as growth, so the whistle can blow on it. `knownLE` orders a `Nat` by value, an `Int` by absolute value and a `Text` by size. Any two `Float`s couple, and anything else couples only when equal.

### Budgets

The whistle is a heuristic, so hard limits back it up: steps of the partial evaluation walk (200,000), residual functions (500), unfolding depth (200), and the size of the generated code, set to a multiple of the size of the input function and its known values. Hitting the step or function limit means giving up, and returning the original closure unchanged, which is always correct. Hitting the depth limit residualizes the call instead, and hitting the size limit first retries with no handler fused (see [Failing safely](#failing-safely)).

## What runs at specialization time

When everything a piece of code needs is known, the specializer does not walk it. It runs it on the real interpreter, which ensures the evaluator has full coverage of the language, and the interpreter is also more efficient at running code than the partial evaluator.

```haskell
-- A term whose free variables are all known, run on the interpreter.
runIfKnown :: SpecEnv -> E -> Term -> M (Maybe Val)
```

Running code during partial evaluation must not move, repeat or drop the program's effects, so it is guarded:

- **Only pure code runs.** A run is refused if it would perform an effect or touch a mutable value (a `Ref`, an `MVar`, a handle). Those are treated as known but opaque: they can be passed around and stored, and every operation on them stays in the output.
- **Runs are bounded in time.** A known computation that would not finish is cut off, and the code stays in the output for run time to deal with.
- **A failing run is not an error.** If known code would crash (`List.at!` out of range under a branch that never executes), the specializer emits the code instead. The crash happens at run time, if that path is ever taken, as it would have without `specialize`.

## Abilities: removing handlers

`handle` blocks and ability requests get specialized as well. How this is done is fairly involved; it’s about half of the specializer's code.

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

A `handle` block is specialized in one of three ways. They are tried in this order, and each is the fallback for the one before it:

1. **Everything is known:** the block is run.
2. **The handler is known:** the handler is specialized away in the output.
3. **Otherwise:** the handler stays, and the code inside the block is specialized.

The examples below take a `()` as their first argument, so that `specialize f ()` specializes `f` with nothing else known.

### 1. Everything known: it runs

A `handle` block whose free variables are all known is run on the interpreter, like any other known code. `runSt 2 do count 4` becomes `12`.

### 2. The handler is known: it is fused away

If the handler is a known function, the specializer tries to remove the `handle` from the output entirely. This is called *fusing* the handler.

The idea is that a request is a call to the handler, passing it the rest of the block as a continuation. When the handler is a known function, that call can be unfolded like any other call.

So no `handle` is emitted. The partial evaluator records which handler is in force and evaluates the block. When it reaches a request, it does what the runtime would do, but does it while specializing:

1. **It calls the handler on the request.** The request is a known value (`SReq`): which ability, which operation, and the arguments, each known or not.
2. **The handler matches on the request.** This is a match on a known value, so one case is picked and the match disappears.
3. **The case's continuation `k` stands for the rest of the block** after the request (`SKont`). The partial evaluator already has this in hand: it is its own `K` at the request. This is another reason `partialEval` is in continuation-passing style.
4. **When the case calls `k v`,** the partial evaluator goes on with the rest of the block, with `v` as the result of the request.

Here is an example, with `s` and `n` unknown:

```unison
straight _ s n = runSt s do
  St.put (St.get + n)
  St.put (St.get * 2)
  St.get
```

`runSt` is unfolded, and the handler in force is `go s`. Then, request by request:

- **`St.get`.** `go s` is called on it, which picks the case `handle k s with go s`. So the block goes on with `s` as the result of `St.get`, still under `go s`. Nothing is emitted.
- **`St.get + n`** is now `s + n`. Both are unknown, so the addition is emitted, and its result is a variable `v1`.
- **`St.put v1`.** `go s` is called on it, which picks `handle k () with go s'`, with `s'` being `v1`. So the block goes on with `()`, and the handler in force is now `go v1`. Nothing is emitted.
- **The second line** goes the same way, under `go v1`: `St.get` is `v1`, the multiplication is emitted, and the handler in force becomes `go` of its result.
- **The last `St.get`** is that result, and it is the value of the block. The handler's `{ a } -> a` case returns it.

This is `runtime.ir (specialize straight ())`:

```
straight.specialized#01c6j7uqg1 _ s n =
  v1 = s Nat.+ n
  lit = 2
  v1 Nat.* lit
```

Notice there are no requests, no continuation capture and no handler closures appearing anywhere here. The state has become an ordinary variable.

Both of `runSt`'s cases resume with `handle k v with h'` as the last thing they do. A case can do two other things with its continuation `k`:

- **Not call it.** The rest of the block is dropped, and the value of the case is the value of the whole `handle`.
- **Call it and then do more work.** The rest of the block is evaluated, and its value comes back to the case, which carries on.

An abort handler is the first kind, and fusing it turns each `abort` into a branch:

```unison
ability Abort where
  abort : a

safeDiv : Nat -> Nat ->{Abort} Nat
safeDiv a b = if b == 0 then abort else a / b

orElse : a -> '{g, Abort} a ->{g} a
orElse d p = handle p () with cases
  { a } -> a
  { abort -> _ } -> d

divs _ a b = orElse 0 do safeDiv a b + safeDiv b a
```

This is `runtime.ir (specialize divs ())`. Where `abort` was, the output is just `0`, the value of the whole `handle`:

```
divs.specialized#02eagb7st1 _ a b =
  lit = 0
  v1 = b == lit
  match v1 with
    false ->
      r = a Nat./ b
      lit1 = 0
      v2 = a == lit1
      match v2 with
        false ->
          r1 = b Nat./ a
          r Nat.+ r1
        _ -> 0
    _ -> 0
```

That is all there is to fusing when the block that the handler surrounds is straight-line code. Three things need more: a loop, a match on an unknown value, and a call to an unknown function. Each has its own section below.

### A loop inside a fused handler

Take `runSt s do count n` with `n` unknown. `count` recurses on an unknown `n`, so it can't be unfolded forever. As in [Recursion and termination](#recursion-and-termination), it has to become a residual function, `count'`, that calls itself.

But `count'` makes `St.get` and `St.put` requests, and the specialized code has no handler to send them to. So what happens to these requests?

The handler in force is a closure, here `go s`. We call the values it closes over, here `s`, the *handler's state*. The loop changes it on every round, and the caller needs to know what it is after the loop, to go on handling requests.

So the residual function takes the handler's state as an extra parameter, and returns the state along with its result. This is `runtime.ir (specialize loop ())`, for `loop _ s n = runSt s do count n`:

```
loop.specialized#02n52n07r1 _ s n =
  count' n1 s2 =
    lit = 0
    v1 = n1 == lit
    match v1 with
      false ->
        v2 = s2 Nat.+ n1
        lit1 = 1
        v3 = n1 Nat.- lit1
        count' v3 v2
      _ ->
        p1 = (s2, s2)
        Right p1
  r = count' n s
  match r with
    Right p ->
      match p with
        Cons r1 rest ->
          match rest with
            Cons s1 rest1 -> r1
    Left done -> done
```

- **`count' n1 s2`** has one more parameter than `count`: `s2`, the handler's state on entry.
- **Inside it, the handler is fused** as in the straight-line case, starting from `go s2`. `St.put (St.get + n)` became `v2 = s2 + n1`, and the handler's state is then `v2`.
- **The recursive call passes the state along:** `count' v3 v2`.
- **At the end it returns `Right (result, state)`.** Here that is `(s2, s2)`, because the result is `St.get`, which is the state.
- **The caller matches on what comes back.** On `Right (r1, s1)` it goes on with the rest of its block, fused, with `r1` as the result of the call and `go s1` as the handler in force. Here nothing is left to do, so it returns `r1`.
- **`Left done` is for a case that doesn't call `k`.** Such a case ends the whole `handle`, not just the loop, so the loop returns the `handle`'s value as `Left`, and the caller returns it without going on. `runSt` has no such case, so `count'` never returns `Left`. Under `orElse`, a loop returns `Left` where it aborts.

This only works when every case that resumes does it as `handle k v with h'`, as the last thing it does. Then "the handler's state after the loop" is a meaningful thing to return.

A case that does more work after resuming, or resumes twice, is different: the rest of the block runs in the middle of the case. Here is a handler that counts `tick`s, whose case adds one after resuming:

```unison
ability Tick where
  tick : ()

runTick : '{Tick} a -> (Nat, a)
runTick p =
  go = cases
    { a } -> (0, a)
    { tick -> k } -> match handle k () with go with
      (n, a) -> (n + 1, a)
  handle p () with go

ticks : Nat ->{Tick} Nat
ticks n = if n == 0 then 0 else
  tick
  ticks (n - 1) + n
```

For such a handler, the loop's residual function takes the rest of its caller's block as a function parameter, and returns the value of the whole `handle`. This is the loop's part of `runtime.ir (specialize (z n -> runTick do ticks (z + n)) 0)`:

```
ticks' n1 k1 =
  lit1 = 0
  v2 = n1 == lit1
  match v2 with
    false ->
      lit2 = 1
      v3 = n1 Nat.- lit2
      k2 = partialApply resume1 n1 k1
      r1 = ticks' v3 k2
      match r1 with
        Cons n2 p1 ->
          match p1 with
            Cons a p2 ->
              match p2 with
                () ->
                  lit3 = 1
                  v4 = n2 Nat.+ lit3
                  (v4, a)
    _ ->
      lit4 = 0
      k1 lit4
resume1 n1 k1 r2 =
  r3 = r2 Nat.+ n1
  k1 r3
```

`k1` is what the caller does with the result of `ticks`. The recursive call passes `k2`, which adds `n1` and then calls `k1`. The `(v4, a)` after the call is the handler's case adding one. There are still no requests and no handler, but each round allocates a closure for `k2`.

### A match on an unknown value inside a fused handler

Consider a block with an `if` on an unknown `b`, and more code after it:

```unison
joined _ s b n = runSt s do
  if b then St.put (St.get + n) else St.put 7
  St.get * n
```

Outside a fused handler, a match on an unknown value is emitted with its arms, and the code after it follows the match once. Here that doesn't work. The code after the `if` does a `St.get`, and what that gives depends on which arm ran: `s + n` after one, `7` after the other.

There are two ways to deal with this, and the specializer uses both:

- **Copy the code after the match into each arm.** Each copy is specialized for the state its arm ends with. This gives the best code, but a block with several `if`s in a row doubles in size at each one.
- **Make the code after the match a function,** called at the end of each arm. This is a *join point*. Its parameters are the things that differ between the arms: here, the handler's state.

The specializer first makes a join point. When it is done, a join point that is small is copied back into the arms that call it. That is what happens to `joined`. This is `runtime.ir (specialize joined ())`:

```
joined.specialized#03tf31lvge _ s b n =
  match b with
    false ->
      lit = 7
      lit Nat.* n
    _ ->
      v1 = s Nat.+ n
      v1 Nat.* n
```

Had the code after the `if` been large, the join point would have stayed a function, and the output would have had this shape:

```
joined _ s b n =
  join s1 = s1 Nat.* n
  match b with
    false -> join 7
    _ ->
      v1 = s Nat.+ n
      join v1
```

An arm that doesn't reach the end (it aborts, say) doesn't call the join point.

### A call to an unknown function inside a fused handler: the bridge

Consider a block that calls an unknown function `f`:

```unison
viaU _ s f = runSt s do
  St.put (f St.get)
  St.get
```

`f` can make `St` requests when it runs. The specializer can't see them, because it doesn't have `f`'s code. And the output has no handler to catch them, because the handler was fused away.

So a real handler is put around that one call, and nowhere else. It is called a *bridge*. It is made from the fused handler's own cases, and it gives back what a loop gives back: `Right (result, state)`, or `Left value` if a case didn't call `k`. So the code after the call goes on fused, the same way as after a loop.

This is `runtime.ir (specialize viaU ())`:

```
viaU.specialized#001il09tv9 _ s f =
  bridge' s2 request =
    match request with
      {put s'} ->
        cont1 = shift {St}
        con = ()
        bridge1 = partialApply bridge' s'
        handle {St}
          cont1 con
        with bridge1
      {St.get} ->
        cont2 = shift {St}
        bridge2 = partialApply bridge' s2
        handle {St}
          cont2 s2
        with bridge2
      {r2} ->
        p1 = (r2, s2)
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
```

- **`f s`:** the `St.get` before the call was fused, so the argument is just `s`.
- **`handle {St} f s with bridge`** is the only handler in the output. It is in force only while `f` runs.
- **`bridge' s2 request`** is `runSt`'s `go`, with one change: when `f` returns `r2`, it returns `Right (r2, s2)`, the result and the state `f` left behind.
- **After the call,** `St.put r1` and the last `St.get` are fused: the block's value is `r1`.

A bridge can only be made for a handler whose cases resume as the last thing they do, like `runSt`. For other handlers, an unknown call in the block means the handler can't be fused, and it is kept (next section).

### 3. Otherwise: the handler stays

When the handler isn't a known function, or fusing it doesn't work out (see [Failing safely](#failing-safely)), the `handle` stays in the output. It is an ordinary handler, installed at run time.

The code inside the handle block is still partially evaluated as usual. The only difference is that an ability request in it is emitted as a request, and the handler handles it at run time.

Here is a handler like `runTick`, around a call to an unknown `f`. Its case does work after resuming, so there is no bridge, and the handler is kept:

```unison
countTicks : '{g, Tick} a ->{g} Nat
countTicks p =
  h = cases
    { _ } -> 0
    { tick -> k } -> 1 + (handle k () with h)
  handle p () with h

keptEx _ f n = countTicks do
  x = f n
  if x == 0 then tick else ()
```

This is `runtime.ir (specialize keptEx ())`. `handled` is the block, `fn` is the handler `h`, and `tick` is still a request:

```
keptEx.specialized#0059hkhcm5 _ f n =
  handled f n =
    x = f n
    lit = 0
    v1 = x == lit
    match v1 with
      false -> ()
      _ -> tick
  fn v2 =
    match v2 with
      {tick} ->
        cont1 = shift {Tick}
        clo1 = partialApply fn
        v3 =
          handle {Tick}
            con = ()
            cont1 con
          with clo1
        lit1 = 1
        lit1 Nat.+ v3
      {p1} -> 0
  clo = partialApply fn
  handle {Tick}
    handled f n
  with clo
```

This level alone is often enough. Take an interpreter whose `eval` calls `abort` when a division is by zero, run under a handler for `Abort`. Specializing it on a program removes all the dispatch on the program, as before. What is left is the program's arithmetic, with an `abort` request where a division can fail, inside one `handle`.

## Failing safely

Fusing a handler is a bet that the walker will see every request made in the handler's scope. When it finds it cannot, it backs out. There are three levels of retreat, each of which still produces correct code.

```haskell
data Escape = Escape Int String   -- back to the fused handler with this number
data GiveUp = GiveUp String       -- abandon this specialization
```

- **`Escape`: keep one handler.** Something in a fused handler's scope could make its requests unseen: a closure escaping as a value, a builtin that calls back into Unison code, a case shape the bridge does not cover. The walker throws `Escape` to that handler's `handle`. The walk's state is rolled back to that point and the `handle` is redone as a kept handler (level 3 above). Everything outside it is unaffected, and other handlers stay fused.
- **Retry with no fusion.** If the generated code exceeds its size budget with handlers fused, the whole specialization is redone once with every handler kept. Fused handlers are what copy code.
- **`GiveUp`: return the original.** If a budget runs out anyway, `specialize f x` returns `f x` unchanged.

Here's what this looks like in code, with the rollback logic:

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
to `f x` and hash differently.

- **It is an ordinary closure** over a generated function, so equality, ordering, hashing and
  serialization need no special case. The generated function's reference is a hash of its code,
  the one `Code.validateLinks` computes.
- **Specializing the same closure twice does the work once**, and gives equal closures. The
  runtime remembers what it made of each closure, by a hash of the closure. This holds within a
  process: another process, or another version of the specializer, may generate different code,
  and then the closures are unequal.
- **Serialization ships the specialized closure** as it is. `Code.lookup` finds the generated
  code like any other loaded code, so the usual code sync sends it to a receiver that lacks it.
  The receiver does not specialize again.
- **Showing it as `specialize f x`** uses a table the runtime keeps of where each generated
  function came from. The table stays in the process that specialized. Elsewhere the closure is
  shown by the generated function's hash.
- **When the specializer gives up**, `specialize f x` is `f x` itself.
- **Mutable values are known but opaque.** A `Ref` or `MVar` in `x` is passed along and shared
  with `f x`, and every operation on it stays in the output. Closures that differ only in which
  mutable value they hold share the generated code.
- **When `f x` isn't a closure**, including when it has run to a plain value, it is returned as
  it is.

See `specializeClosure`, `closureKey` and `hashedGroup` in [`Machine.hs`](../../Machine.hs).

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
specialized. To get the best results:

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
- **The whistle's cost.** The embedding test first runs with no table for a fixed number of steps (`embedsWithin`), which decides nearly every pair. Past that it keeps its answer for each pair of subterms (`embedsTabled`), so it takes time proportional to size(a) × size(b). Known values are compared by address before they are compared by structure. If that ever matters, a tag-bag whistle compares multisets of source-location tags instead of trees (Mitchell 2010; Bolingbroke and Peyton Jones 2010). Tag-bags are cheaper but coarser, so they may generalize sooner.
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
| The builtin, closure identity and serialization | `specializeClosure`, `closureKey`, `hashedGroup` in `Machine.hs` |

The transcripts `unison-src/transcripts/idempotent/specialize.md` and `specialize-differential.md` are the best place to see real input and output side by side.
