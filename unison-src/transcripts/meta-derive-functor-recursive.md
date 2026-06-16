# Derive Functor for recursive types

The previous derivers handled the easy fields — bare `a`-typed
parameters get `f`-applied, everything else passes through. That's
correct for `Optional a` and `Tagged Text a`, but wrong for any
constructor that holds a recursive value of the same type. For
@Tree a = Leaf | Node a (Tree a) (Tree a)@ the deriver needs to
emit recursive `map` calls on the two `Tree a` fields, not pass
them unchanged.

To do that the deriver builds the map function as a `letrec` so it
can call itself by name, and the field-dispatch logic learns one
new shape: @App (Ref selfTypeRef) (Var _)@ → recursive call.

```ucm :hide
scratch/main> builtins.mergeio
```

## The class

```unison
unique type Functor f = Functor (forall a b. (a -> b) -> f a -> f b)
```

```ucm
scratch/main> add
```

## meta.Term builders

The new `mkLetRec` builds @Cycle (Abs name (LetRec [body] (Var name)))@
— a self-recursive single-binding letrec.

```unison
emptySet : Set meta.Name
emptySet = Set.Set Map.Tip

List.replicate : Nat -> a -> [a]
List.replicate n x =
  if n == 0 then []
  else x +: List.replicate (Nat.drop n 1) x

List.range : Nat -> Nat -> [Nat]
List.range lo hi =
  if lo == hi then []
  else lo +: List.range (lo + 1) hi

List.foldRight : (a -> b -> b) -> b -> [a] -> b
List.foldRight f acc xs = match xs with
  [] -> acc
  h +: t -> f h (List.foldRight f acc t)

List.foldl : (b -> a -> b) -> b -> [a] -> b
List.foldl f acc xs = match xs with
  [] -> acc
  h +: t -> List.foldl f (f acc h) t

List.map : (a -> b) -> [a] -> [b]
List.map f xs = match xs with
  [] -> []
  h +: t -> f h +: List.map f t

List.zip : [a] -> [b] -> [(a, b)]
List.zip xs ys = match (xs, ys) with
  (xh +: xt, yh +: yt) -> (xh, yh) +: List.zip xt yt
  _ -> []

mkTm : meta.TermF (meta.Term meta.TermF) -> meta.Term meta.TermF
mkTm tf = meta.Term.Term emptySet (meta.ABT.Tm tf)

mkVar : Text -> meta.Term meta.TermF
mkVar name = meta.Term.Term emptySet (meta.ABT.Var (meta.Name.Name name))

mkAbs : Text -> meta.Term meta.TermF -> meta.Term meta.TermF
mkAbs name body =
  meta.Term.Term emptySet (meta.ABT.Abs (meta.Name.Name name) body)

mkApp : meta.Term meta.TermF -> meta.Term meta.TermF -> meta.Term meta.TermF
mkApp f x = mkTm (meta.TermF.App f x)

mkLam : Text -> meta.Term meta.TermF -> meta.Term meta.TermF
mkLam binder body = mkTm (meta.TermF.Lam (mkAbs binder body))

mkConstructor : meta.ConstructorReference -> meta.Term meta.TermF
mkConstructor cr = mkTm (meta.TermF.Constructor cr)

mkMatch :
  meta.Term meta.TermF
  -> [meta.MatchCase (meta.Term meta.TermF)]
  -> meta.Term meta.TermF
mkMatch scrut clauses = mkTm (meta.TermF.Match scrut clauses)

-- `letrec name = body in name`. The Cycle/Abs/LetRec/Var nesting
-- mirrors what Term.letRec produces in Haskell.
mkLetRec : Text -> meta.Term meta.TermF -> meta.Term meta.TermF
mkLetRec name body =
  letRecNode =
    mkTm (meta.TermF.LetRec [body] (mkVar name))
  meta.Term.Term emptySet
    (meta.ABT.Cycle
      (meta.Term.Term emptySet
        (meta.ABT.Abs (meta.Name.Name name) letRecNode)))
```

```ucm
scratch/main> add
```

## Type-shape checks: parameter vs. recursive

`isVarType` recognises a bare `Var x` — the mapped parameter.
`isRecursiveType selfRef` recognises `App (Ref selfRef) (Var _)` —
a recursive field of the same type we're deriving for.

```unison
isVarType : meta.Term meta.TypeF -> Boolean
isVarType ty = match ty with
  meta.Term.Term _ (meta.ABT.Var _) -> true
  _ -> false

referenceEq : meta.Reference -> meta.Reference -> Boolean
referenceEq a b = match (a, b) with
  ( meta.Reference.ReferenceBuiltin x,
    meta.Reference.ReferenceBuiltin y ) -> x == y
  ( meta.Reference.ReferenceDerived (meta.Hash.Hash hx) ix,
    meta.Reference.ReferenceDerived (meta.Hash.Hash hy) iy ) ->
      hx == hy && ix == iy
  _ -> false

isRecursiveType : meta.Reference -> meta.Term meta.TypeF -> Boolean
isRecursiveType selfRef ty = match ty with
  meta.Term.Term _
    (meta.ABT.Tm
      (meta.TypeF.App
        (meta.Term.Term _ (meta.ABT.Tm (meta.TypeF.Ref r)))
        _)) ->
      referenceEq r selfRef
  _ -> false
```

```ucm
scratch/main> add
```

## The deriver

Three field shapes per constructor field:

* bare `Var` — apply `f` (mapped parameter)
* `App (Ref selfRef) (Var _)` — recursive call `myMap f x`
* anything else — pass through

```unison
mkFieldExpr :
  meta.Reference
  -> (Text, meta.Term meta.TypeF)
  -> meta.Term meta.TermF
mkFieldExpr selfRef tup = match tup with (varName, fieldType) ->
  if isVarType fieldType then
    mkApp (mkVar "f") (mkVar varName)
  else if isRecursiveType selfRef fieldType then
    mkApp (mkApp (mkVar "myMap") (mkVar "f")) (mkVar varName)
  else
    mkVar varName

mkFunctorCase :
  meta.Reference
  -> (meta.ConstructorReference, [meta.Term meta.TypeF])
  -> meta.MatchCase (meta.Term meta.TermF)
mkFunctorCase selfRef tup = match tup with (cr, fieldTypes) ->
  ctorRef = match cr with
    meta.ConstructorReference.ConstructorReference r _ -> r
  ctorCid = match cr with
    meta.ConstructorReference.ConstructorReference _ cid -> cid
  arity = List.size fieldTypes
  pat = meta.Pattern.PConstructor ctorRef ctorCid
          (List.replicate arity meta.Pattern.PVar)
  varNames =
    List.map (n -> "x" ++ Nat.toText n) (List.range 0 arity)
  pairs = List.zip varNames fieldTypes
  argExprs = List.map (mkFieldExpr selfRef) pairs
  step acc arg = mkApp acc arg
  bodyApplied = List.foldl step (mkConstructor cr) argExprs
  bodyWithAbs = List.foldRight mkAbs bodyApplied varNames
  meta.MatchCase.MatchCase pat None bodyWithAbs

-- Wrap the map function in a letrec so recursive fields can call it
-- by the name "myMap".
deriveMap : meta.Reference ->{IO} Optional (meta.Term meta.TermF)
deriveMap typeRef =
  match Meta.dataDeclShape typeRef with
    None -> None
    Some ctors ->
      clauses = List.map (mkFunctorCase typeRef) ctors
      mapBody =
        mkLam "f" (mkLam "x" (mkMatch (mkVar "x") clauses))
      Some (mkLetRec "myMap" mapBody)
```

```ucm
scratch/main> add
```

## Sample-driven type-reference lookup

```unison
headRef : meta.Term meta.TermF -> Optional meta.Reference
headRef tm = match tm with
  meta.Term.Term _ abt -> match abt with
    meta.ABT.Tm tf -> match tf with
      meta.TermF.Constructor
        (meta.ConstructorReference.ConstructorReference r _) -> Some r
      meta.TermF.App f _ -> headRef f
      _ -> None
    _ -> None

typeRefOf : a ->{IO} Optional meta.Reference
typeRefOf sample = headRef (Meta.decompile sample)
```

```ucm
scratch/main> add
```

## A recursive type

```unison
unique type Tree a = Leaf | Node a (Tree a) (Tree a)
```

```ucm
scratch/main> add
```

## Derive and store

```unison
storeFunctorTree : '{IO} Either Text Link.Term
storeFunctorTree _ = match typeRefOf (Tree.Leaf : Tree Nat) with
  None -> Left "couldn't get type ref"
  Some treeRef -> match deriveMap treeRef with
    None -> Left "Meta.dataDeclShape returned None"
    Some mapAst -> Meta.store [| Functor.Functor ${mapAst} |]
```

```ucm
scratch/main> add
scratch/main> run storeFunctorTree
```

## Alias, view, run

```ucm
scratch/main> alias.term #2ekfm0gl3d Functor.tree
scratch/main> view Functor.tree
scratch/main> mark.given Functor.tree
```

`view Functor.tree` reifies the derived definition — note the
`let rec` and the self-application on the two `Tree a` fields of
`Node`.

```unison
sampleTree : Tree Nat
sampleTree = Node 1 (Node 2 Tree.Leaf Tree.Leaf) (Node 3 Tree.Leaf (Node 4 Tree.Leaf Tree.Leaf))

runTreeMap : '{IO} Tree Nat
runTreeMap _ = match Functor.tree with
  Functor.Functor m -> m (n -> n Nat.* 10) sampleTree
```

```ucm
scratch/main> add
scratch/main> run runTreeMap
```
