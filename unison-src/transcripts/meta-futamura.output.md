# The Futamura projections in Unison

In 1971 [Yoshihiko Futamura](https://en.wikipedia.org/wiki/Partial_evaluation#Futamura_projections) noticed something extraordinary
about partial evaluation. If you have a partial evaluator `mix` and
an interpreter `evalExpr` for some language, then:

1.  `mix(evalExpr, source)` is **the source program, compiled** — the
    first projection.
2.  `mix(mix, evalExpr)` is **a compiler** for that language — the
    second projection.
3.  `mix(mix, mix)` is **a compiler generator** — the third projection.

The astonishing claim is that you can recover a compiler from an
interpreter, and a compiler-generator from a partial evaluator,
mechanically. No new code, just specialization.

This transcript walks the projections in Unison. The punchline up
front: **with first-class macros, the projections collapse**. You
don't need `mix(mix, mix)` — quasiquotation is the specializer, and
it works at every level for free.

``` ucm :hide
scratch/main> builtins.mergeio
```

## The language

A tiny polymorphic expression language: literals of some type `a`,
variables, two binary operations, and let-bindings.

``` unison
unique type BinOp = OpAdd | OpMul

unique type Expr a
  = ELit a
  | EVar Text
  | EBin BinOp (Expr a) (Expr a)
  | ELet Text (Expr a) (Expr a)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type BinOp
  + type Expr a

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Projection 0 — the interpreter

What you'd write without macros: an interpreter that walks the AST
at every call. Slow. The interpretation overhead is baked into the
hot path of every program written in the DSL.

``` unison
lookupEnv : [(Text, a)] -> Text -> a
lookupEnv env name = match env with
  [] -> bug ("unbound variable " ++ name)
  pair +: rest -> match pair with
    (n, v) -> if n == name then v else lookupEnv rest name

evalExpr : (BinOp -> a -> a -> a) -> [(Text, a)] -> Expr a -> a
evalExpr op env = cases
  ELit n -> n
  EVar name -> lookupEnv env name
  EBin o a b -> op o (evalExpr op env a) (evalExpr op env b)
  ELet name e body ->
    v = evalExpr op env e
    evalExpr op ((name, v) +: env) body

natOp : BinOp -> Nat -> Nat -> Nat
natOp = cases
  OpAdd -> (Nat.+)
  OpMul -> (Nat.*)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + evalExpr  : (BinOp ->{g2} a ->{g1} a ->{g} a)
                -> [(Text, a)]
                -> Expr a
                ->{g, g1, g2} a
  + lookupEnv : [(Text, a)] -> Text -> a
  + natOp     : BinOp -> Nat -> Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Sanity check — `let z = x + y in z * z` with `x = 3, y = 4`:

``` unison
sampleExpr : Expr a
sampleExpr =
  ELet "z" (EBin OpAdd (EVar "x") (EVar "y"))
    (EBin OpMul (EVar "z") (EVar "z"))

evalExprDemo : '{IO, Exception} Nat
evalExprDemo _ = evalExpr natOp [("x", 3), ("y", 4)] sampleExpr
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + evalExprDemo : '{IO, Exception} Nat
  + sampleExpr   : Expr a

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run evalExprDemo

  49
```

`(3 + 4)² = 49`. The interpreter works. It's also the slow path.

## Projection 1 — specialize the interpreter to one program

`stage` walks an `Expr` and emits a quoted Unison expression that
computes the same value, **with no interpreter on the call path**.
This is `mix(evalExpr, source)` in Futamura's first projection — and
in our setting it's just a Unison function returning a `meta.Term`.

``` unison
emptySet : Set meta.Name
emptySet = Set.Set Map.Tip

mkVar : Text -> meta.Term meta.TermF
mkVar name =
  meta.Term.Term emptySet (meta.ABT.Var (meta.Name.Name name))

wrapLam : Text -> meta.Term meta.TermF -> meta.Term meta.TermF
wrapLam name body =
  absNode =
    meta.Term.Term emptySet (meta.ABT.Abs (meta.Name.Name name) body)
  meta.Term.Term emptySet
    (meta.ABT.Tm (meta.TermF.Lam absNode))

mkLet :
  Text
  -> meta.Term meta.TermF
  -> meta.Term meta.TermF
  -> meta.Term meta.TermF
mkLet name binding body =
  absNode =
    meta.Term.Term emptySet (meta.ABT.Abs (meta.Name.Name name) body)
  meta.Term.Term emptySet
    (meta.ABT.Tm (meta.TermF.Let binding absNode))

stageNat : Expr Nat ->{IO} meta.Term meta.TermF
stageNat = cases
  ELit n -> Meta.decompile n
  EVar name -> mkVar name
  EBin op a b ->
    a' = stageNat a
    b' = stageNat b
    match op with
      OpAdd -> [| ${a'} Nat.+ ${b'} |]
      OpMul -> [| ${a'} Nat.* ${b'} |]
  ELet name e body ->
    mkLet name (stageNat e) (stageNat body)

wrapLams : [Text] -> meta.Term meta.TermF -> meta.Term meta.TermF
wrapLams params body = match params with
  [] -> body
  h +: t -> wrapLam h (wrapLams t body)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + emptySet : Set Name
  + mkLet    : Text
               -> meta.Term TermF
               -> meta.Term TermF
               -> meta.Term TermF
  + mkVar    : Text -> meta.Term TermF
  + stageNat : Expr Nat ->{IO} meta.Term TermF
  + wrapLam  : Text -> meta.Term TermF -> meta.Term TermF
  + wrapLams : [Text] -> meta.Term TermF -> meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Compile the sample expression with `["x", "y"]` as the parameter
names, store the result under the name `polyNat`, and view it. The
`storeAt` helper calls `Meta.store` and, on success, binds the
resulting `Link.Term` to a name via `Meta.aliasTerm` — all in one
IO action, no UCM dance required.

``` unison
storeAt : Text -> meta.Term meta.TermF ->{IO} Either Text Link.Term
storeAt name tm =
  match Meta.store tm with
    Left e -> Left e
    Right link ->
      _ = Meta.aliasTerm link name
      Right link

makePolyNat : '{IO} Either Text Link.Term
makePolyNat _ =
  body = stageNat sampleExpr
  storeAt "polyNat" (wrapLams ["x", "y"] body)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + makePolyNat : '{IO} Either Text Link.Term
  + storeAt     : Text
                  -> meta.Term TermF
                  ->{IO} Either Text Link.Term

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run makePolyNat

  Right (termLink #t6nbn09tsd)

scratch/main> view polyNat

  polyNat : Nat -> Nat -> Nat
  polyNat x y =
    use Nat * +
    z = x + y
    z * z
```

The `Expr` AST is **erased**. What's left is ordinary Unison — the
let binding survived, the operators are real `Nat.+` / `Nat.*`, the
signature was inferred from the use sites. Call it directly:

``` unison
runPolyNat : '{IO, Exception} Nat
runPolyNat _ = polyNat 3 4
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + runPolyNat : '{IO, Exception} Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run runPolyNat

  49
```

Same answer as the interpreter — `49` — but now it's just arithmetic.

## Projection 2 — specialize the specializer to an interpreter

The second projection promises a **compiler**, not a compiled
program. Same input language, but instead of feeding one specific
source program through, we feed the *language description* through
and get back a function that compiles any source program in that
language.

In a classical setting this requires self-applying `mix`. In Unison
it's just a parameter: factor `stageNat`'s `OpAdd → Nat.+ / OpMul → Nat.*` table out as data.

``` unison
unique type Compiler =
  Compiler
    ( BinOp
      -> meta.Term meta.TermF
      -> meta.Term meta.TermF
      -> meta.Term meta.TermF
    )

stageWith : Compiler -> Expr a ->{IO} meta.Term meta.TermF
stageWith spec = cases
  ELit n -> Meta.decompile n
  EVar name -> mkVar name
  EBin op a b ->
    a' = stageWith spec a
    b' = stageWith spec b
    match spec with Compiler emit -> emit op a' b'
  ELet name e body ->
    mkLet name (stageWith spec e) (stageWith spec body)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Compiler

  + stageWith : Compiler -> Expr a ->{IO} meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Three different compilers — three different ways of interpreting the
same source language.

``` unison
natCompiler : Compiler
natCompiler =
  Compiler (cases
    OpAdd -> a b -> [| ${a} Nat.+ ${b} |]
    OpMul -> a b -> [| ${a} Nat.* ${b} |])

boolCompiler : Compiler
boolCompiler =
  Compiler (cases
    OpAdd -> a b -> [| ${a} || ${b} |]
    OpMul -> a b -> [| ${a} && ${b} |])

textCompiler : Compiler
textCompiler =
  Compiler (cases
    OpAdd -> a b -> [| ${a} Text.++ ${b} |]
    OpMul -> a b -> [| ${a} Text.++ "·" Text.++ ${b} |])
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + boolCompiler : Compiler
  + natCompiler  : Compiler
  + textCompiler : Compiler

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now compile the **same** `sampleExpr` AST three different ways. Each
call to `stageWith` returns a different compiled program — the
specializer has been specialized to a language.

``` unison
storeForCompiler :
  Text
  -> Compiler
  -> '{IO} Either Text Link.Term
storeForCompiler name c _ =
  body = stageWith c sampleExpr
  storeAt name (wrapLams ["x", "y"] body)

makePolyBool : '{IO} Either Text Link.Term
makePolyBool = storeForCompiler "polyBool" boolCompiler

makePolyText : '{IO} Either Text Link.Term
makePolyText = storeForCompiler "polyText" textCompiler
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + makePolyBool     : '{IO} Either Text Link.Term
  + makePolyText     : '{IO} Either Text Link.Term
  + storeForCompiler : Text
                       -> Compiler
                       -> '{IO} Either Text Link.Term

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run makePolyBool

  Right (termLink #r7hn0813kt)

scratch/main> run makePolyText

  Right (termLink #fa5njj13rg)
```

## The reveal — three compilers, one source, three target programs

``` ucm
scratch/main> view polyNat

  polyNat : Nat -> Nat -> Nat
  polyNat x y =
    use Nat * +
    z = x + y
    z * z

scratch/main> view polyBool

  polyBool : Boolean -> Boolean -> Boolean
  polyBool x y =
    z = if x then true else y
    if z then z else false

scratch/main> view polyText

  polyText : Text -> Text -> Text
  polyText x y =
    use Text ++
    z = x ++ y
    z ++ "·" ++ z
```

Three completely different compiled programs — `Nat -> Nat -> Nat`,
`Boolean -> Boolean -> Boolean`, `Text -> Text -> Text` — produced
by feeding the **same** `sampleExpr` AST through `stageWith` with
three different `Compiler` values. The `Expr` AST is erased in each
one; the let binding survives; the operators are inlined. Run them
to confirm they actually do what their signatures say:

``` unison
runAllThree : '{IO, Exception} (Nat, Boolean, Text)
runAllThree _ =
  ( polyNat 3 4,
    polyBool true false,
    polyText "ab" "c" )
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + runAllThree : '{IO, Exception} (Nat, Boolean, Text)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run runAllThree

  (49, true, "abc·abc")
```

## Projection 3 — and the collapse

The third projection is `mix(mix, mix)` — specializing the
specializer to itself, recovering a *compiler-generator*. A function
that, given any interpreter, produces a compiler for it.

In our setting, look at the type signature again:

``` 
stageWith : Compiler -> Expr a ->{IO} meta.Term meta.TermF
```

`stageWith` is **already** a compiler-generator. Given any
`Compiler` value, it returns a compiler `Expr a -> {IO} meta.Term meta.TermF`. There is no `mix(mix, mix)` step to take, because there
was never a separate `mix` to apply — the specializer is built into
the language as quasiquotation, and quasiquotation commutes through
every level of abstraction we care about.

This is what makes first-class macros special. The three Futamura
projections collapse into one fact: **if you can construct ASTs in
your source language, you can write any of these specializers as
ordinary functions, and partial evaluation is what they do for a
living.**

The thing the projections classically buy you — automatic
specialization of an existing interpreter without rewriting it — is
genuinely valuable when you can't change the interpreter. In Unison,
you write the staged version directly. The interpreter and the
compiler share most of their structure (compare `evalExpr` vs
`stageWith` above), but here you just write the one you want.

What you don't lose: the **values** the projections deliver — a
compiled program, a compiler for a language, a compiler-generator
parameterized over interpreters — all of those are still
expressible. They're at the top of this file.
