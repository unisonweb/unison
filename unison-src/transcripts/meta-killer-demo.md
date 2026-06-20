# The killer demo — a staged compiler in 30 lines

You have a tiny arithmetic DSL. The usual move: write an interpreter
and pay an interpretation tax on every call. With macros, you write
a **compiler** instead — your DSL expressions become *native Unison
functions in the codebase*, with no interpreter on the call path.

The compiler fits in 30 lines. The compiled functions are
indistinguishable from hand-written code.

```ucm :hide
scratch/main> builtins.mergeio
```

## The DSL

A tiny expression language: numeric literals, variables, addition,
multiplication, and let-bindings.

```unison
unique type Expr
  = ELit Nat
  | EVar Text
  | EAdd Expr Expr
  | EMul Expr Expr
  | ELet Text Expr Expr
```

```ucm
scratch/main> add
```

## The slow path — an interpreter

What you'd write without macros. Walks the AST on every call.

```unison
lookupEnv : [(Text, Nat)] -> Text -> Nat
lookupEnv env name = match env with
  [] -> bug ("unbound variable " ++ name)
  pair +: rest -> match pair with
    (n, v) -> if n == name then v else lookupEnv rest name

interp : [(Text, Nat)] -> Expr -> Nat
interp env = cases
  ELit n -> n
  EVar name -> lookupEnv env name
  EAdd a b -> interp env a Nat.+ interp env b
  EMul a b -> interp env a Nat.* interp env b
  ELet name e body ->
    v = interp env e
    interp ((name, v) +: env) body
```

```ucm
scratch/main> add
```

Sanity check — evaluate `let z = x + y in z * z` with `x = 3, y = 4`,
expect `(3 + 4)² = 49`:

```unison
sampleExpr : Expr
sampleExpr =
  ELet "z" (EAdd (EVar "x") (EVar "y"))
    (EMul (EVar "z") (EVar "z"))

interpDemo : '{IO, Exception} Nat
interpDemo _ = interp [("x", 3), ("y", 4)] sampleExpr
```

```ucm
scratch/main> add
scratch/main> run interpDemo
```

## The fast path — a compiler, in 30 lines

`stage` walks the `Expr` and emits **a quoted Unison expression**
that computes the same value, with zero interpretation overhead.
Variables in `Expr` become free Unison variables; addition/multiplication
become real `Nat.+` / `Nat.*`; let-bindings become real Unison
let-bindings.

```unison
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

stage : Expr ->{IO} meta.Term meta.TermF
stage = cases
  ELit n -> Meta.decompile n
  EVar name -> mkVar name
  EAdd a b ->
    a' = stage a
    b' = stage b
    [| ${a'} Nat.+ ${b'} |]
  EMul a b ->
    a' = stage a
    b' = stage b
    [| ${a'} Nat.* ${b'} |]
  ELet name e body ->
    e' = stage e
    body' = stage body
    mkLet name e' body'

wrapLams : [Text] -> meta.Term meta.TermF -> meta.Term meta.TermF
wrapLams params body = match params with
  [] -> body
  h +: t -> wrapLam h (wrapLams t body)

compile : [Text] -> Expr ->{IO} meta.Term meta.TermF
compile params expr =
  body = stage expr
  wrapLams params body
```

```ucm
scratch/main> add
```

## The "oh my god" moment

Take the sample `Expr` from above. Compile it with parameters
`["x", "y"]`. Store the result. Alias it. **Now view it.**

```unison
storeCompiled : '{IO} Either Text Link.Term
storeCompiled _ = Meta.store (compile ["x", "y"] sampleExpr)
```

```ucm
scratch/main> add
scratch/main> run storeCompiled
scratch/main> alias.term #t6nbn09tsd polyFn
scratch/main> view polyFn
```

That's *real Unison*. No interpreter. No environment lookup. No
ELit / EAdd dispatch. The `Expr` AST has been **erased** —
specialized away into a function the codebase treats like any
other.

Call it directly:

```unison
runCompiled : '{IO, Exception} Nat
runCompiled _ = polyFn 3 4
```

```ucm
scratch/main> add
scratch/main> run runCompiled
```


Same answer as the interpreter — `49` — but now it's just arithmetic.

## A bigger example

Let's stage a more interesting polynomial — `(a + b) * (a + 2) +
(b * b * 3)` — and watch it come out as ordinary Unison:

```unison
biggerExpr : Expr
biggerExpr =
  EAdd
    (EMul (EAdd (EVar "a") (EVar "b"))
          (EAdd (EVar "a") (ELit 2)))
    (EMul (EMul (EVar "b") (EVar "b")) (ELit 3))

storeBigger : '{IO} Either Text Link.Term
storeBigger _ = Meta.store (compile ["a", "b"] biggerExpr)
```

```ucm
scratch/main> add
scratch/main> run storeBigger
scratch/main> alias.term #7j844t0qrm biggerFn
scratch/main> view biggerFn
```

The macro took the `Expr` value and produced a Unison function. The
function's hash is in the codebase. It's a first-class citizen — you
can call it, decompile it, store it as a `given`, refactor it.

This is the [Futamura projection][1] — specializing an interpreter
to its input to recover a compiler — written as an everyday Unison
library, in 30 lines, using values your program builds at runtime.

[1]: https://en.wikipedia.org/wiki/Partial_evaluation#Futamura_projections
