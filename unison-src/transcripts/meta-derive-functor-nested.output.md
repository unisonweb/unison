# Derive Functor where fields are wrapped in other Functors

The recursive deriver handles `Tree a = Leaf | Node a (Tree a) (Tree a)`
because every recursive field shape is `App (Ref selfRef) (Var _)`.
But what about a constructor whose field is wrapped in some *other*
type that's already a Functor? Something like

unique type Search a = Found a | Continue (Optional (Search a))

The `Continue` field is `Optional (Search a)` — not bare `Var`, not
`App (Ref Search) (Var _)`, but `App (Ref Optional) (App (Ref Search) (Var _))`. We'd like the deriver to thread the recursive map through
`Optional`'s own Functor instance.

The trick: write a small helper `mapperFor` that walks the field
type and produces the mapping function. For `Optional (Search a)`
it composes `fmapWith Functor.optional` with the recursive
`mySearchMap`.

``` ucm :hide
scratch/main> builtins.mergeio
```

## Setup: class + an Optional Functor instance

``` unison
unique type Functor f = Functor (forall a b. (a -> b) -> f a -> f b)

fmapWith : Functor f -> (a -> b) -> f a -> f b
fmapWith inst g x = match inst with Functor.Functor m -> m g x

Functor.optional : Functor Optional
Functor.optional = Functor.Functor (f -> cases
  None -> None
  Some x -> Some (f x))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Functor f

  + fmapWith         : Functor f -> (a -> b) -> f a -> f b
  + Functor.optional : Functor Optional

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> mark.given Functor.optional

  Marked Functor.optional. It will now participate in implicit
  resolution.
```

## meta.Term builders

``` unison
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

mkLetRec : Text -> meta.Term meta.TermF -> meta.Term meta.TermF
mkLetRec name body =
  letRecNode = mkTm (meta.TermF.LetRec [body] (mkVar name))
  meta.Term.Term emptySet
    (meta.ABT.Cycle
      (meta.Term.Term emptySet
        (meta.ABT.Abs (meta.Name.Name name) letRecNode)))

identityLam : meta.Term meta.TermF
identityLam = mkLam "y" (mkVar "y")
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + emptySet       : Set Name
  + identityLam    : meta.Term TermF
  + List.foldl     : (b ->{g1} a ->{g} b)
                     -> b
                     -> [a]
                     ->{g, g1} b
  + List.foldRight : (a ->{g1} b ->{g} b)
                     -> b
                     -> [a]
                     ->{g, g1} b
  + List.map       : (a ->{g} b) -> [a] ->{g} [b]
  + List.range     : Nat -> Nat -> [Nat]
  + List.replicate : Nat -> a -> [a]
  + List.zip       : [a] -> [b] -> [(a, b)]
  + mkAbs          : Text -> meta.Term TermF -> meta.Term TermF
  + mkApp          : meta.Term TermF
                     -> meta.Term TermF
                     -> meta.Term TermF
  + mkConstructor  : ConstructorReference -> meta.Term TermF
  + mkLam          : Text -> meta.Term TermF -> meta.Term TermF
  + mkLetRec       : Text -> meta.Term TermF -> meta.Term TermF
  + mkMatch        : meta.Term TermF
                     -> [MatchCase (meta.Term TermF)]
                     -> meta.Term TermF
  + mkTm           : TermF (meta.Term TermF) -> meta.Term TermF
  + mkVar          : Text -> meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Reference helpers

``` unison
referenceEq : meta.Reference -> meta.Reference -> Boolean
referenceEq a b = match (a, b) with
  ( meta.Reference.ReferenceBuiltin x,
    meta.Reference.ReferenceBuiltin y ) -> x == y
  ( meta.Reference.ReferenceDerived (meta.Hash.Hash hx) ix,
    meta.Reference.ReferenceDerived (meta.Hash.Hash hy) iy ) ->
      hx == hy && ix == iy
  _ -> false

findInstance :
  meta.Reference
  -> [(meta.Reference, meta.Reference)]
  -> Optional meta.Reference
findInstance r xs = match xs with
  [] -> None
  pair +: rest -> match pair with (k, v) ->
    if referenceEq k r then Some v
    else findInstance r rest
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + findInstance : Reference
                   -> [(Reference, Reference)]
                   -> Optional Reference
  + referenceEq  : Reference -> Reference -> Boolean

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## The core: `mapperFor`

Recursively walks a field type and produces the mapping function
that maps `f` over a value of that type:

  - `Var _` (the mapped parameter)         → `f`
  - `App (Ref selfRef) inner`              → `myMap (mapperFor inner)`
  - `App (Ref T) inner` where T has Functor → `fmapWith Functor.T (mapperFor inner)`
  - anything else                          → `\y -> y` (identity)

The deriver emits `Ref` nodes (with concrete references obtained via
`Meta.linkRef (termLink ...)`) for the helper functions and instance
references — so the typechecker resolves them by hash, not by free
variable name.

``` unison
mkRef : meta.Reference -> meta.Term meta.TermF
mkRef r = mkTm (meta.TermF.Ref r)

mapperFor :
  meta.Reference                      -- selfRef
  -> meta.Reference                   -- fmapWith's Reference
  -> [(meta.Reference, meta.Reference)] -- typeRef -> instance Ref
  -> meta.Term meta.TypeF
  -> meta.Term meta.TermF
mapperFor selfRef fmapWithRef instances ty = match ty with
  meta.Term.Term _ (meta.ABT.Var _) -> mkVar "f"
  meta.Term.Term _ (meta.ABT.Tm (meta.TypeF.App
    (meta.Term.Term _ (meta.ABT.Tm (meta.TypeF.Ref tRef)))
    inner)) ->
      innerMapper = mapperFor selfRef fmapWithRef instances inner
      if referenceEq tRef selfRef then
        mkApp (mkVar "myMap") innerMapper
      else match findInstance tRef instances with
        Some instRef ->
          mkApp (mkApp (mkRef fmapWithRef) (mkRef instRef)) innerMapper
        None -> identityLam
  _ -> identityLam
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mapperFor : Reference
                -> Reference
                -> [(Reference, Reference)]
                -> meta.Term TypeF
                -> meta.Term TermF
  + mkRef     : Reference -> meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## The deriver

``` unison
mkFieldExpr :
  meta.Reference
  -> meta.Reference
  -> [(meta.Reference, meta.Reference)]
  -> (Text, meta.Term meta.TypeF)
  -> meta.Term meta.TermF
mkFieldExpr selfRef fmapWithRef instances tup =
  match tup with (varName, ty) ->
    mkApp (mapperFor selfRef fmapWithRef instances ty) (mkVar varName)

mkFunctorCase :
  meta.Reference
  -> meta.Reference
  -> [(meta.Reference, meta.Reference)]
  -> (meta.ConstructorReference, [meta.Term meta.TypeF])
  -> meta.MatchCase (meta.Term meta.TermF)
mkFunctorCase selfRef fmapWithRef instances tup =
  match tup with (cr, fieldTypes) ->
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
    argExprs =
      List.map (mkFieldExpr selfRef fmapWithRef instances) pairs
    step acc arg = mkApp acc arg
    bodyApplied = List.foldl step (mkConstructor cr) argExprs
    bodyWithAbs = List.foldRight mkAbs bodyApplied varNames
    meta.MatchCase.MatchCase pat None bodyWithAbs

deriveMap :
  meta.Reference
  -> [(meta.Reference, meta.Reference)]
  -> meta.Reference
  ->{IO} Optional (meta.Term meta.TermF)
deriveMap fmapWithRef instances typeRef =
  match Meta.dataDeclShape typeRef with
    None -> None
    Some ctors ->
      clauses =
        List.map (mkFunctorCase typeRef fmapWithRef instances) ctors
      mapBody =
        mkLam "f" (mkLam "x" (mkMatch (mkVar "x") clauses))
      Some (mkLetRec "myMap" mapBody)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + deriveMap     : Reference
                    -> [(Reference, Reference)]
                    -> Reference
                    ->{IO} Optional (meta.Term TermF)
  + mkFieldExpr   : Reference
                    -> Reference
                    -> [(Reference, Reference)]
                    -> (Text, meta.Term TypeF)
                    -> meta.Term TermF
  + mkFunctorCase : Reference
                    -> Reference
                    -> [(Reference, Reference)]
                    -> (ConstructorReference, [meta.Term TypeF])
                    -> MatchCase (meta.Term TermF)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Type-reference helpers

``` unison
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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + headRef   : meta.Term TermF -> Optional Reference
  + typeRefOf : a ->{IO} Optional Reference

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## A nested recursive type

``` unison
unique type Search a = Found a | Continue (Optional (Search a))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Search a

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Derive Functor for Search

``` unison
storeFunctorSearch : '{IO} Either Text Link.Term
storeFunctorSearch _ =
  match typeRefOf (Search.Found 0) with
    None -> Left "couldn't get type ref for Search"
    Some searchRef ->
      match typeRefOf (Some 0) with
        None -> Left "couldn't get type ref for Optional"
        Some optionalRef ->
          fmapWithRef = Meta.linkRef (termLink fmapWith)
          optionalInstRef = Meta.linkRef (termLink Functor.optional)
          instances = [(optionalRef, optionalInstRef)]
          match deriveMap fmapWithRef instances searchRef with
            None -> Left "Meta.dataDeclShape returned None"
            Some mapAst -> Meta.store [| Functor.Functor ${mapAst} |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + storeFunctorSearch : '{IO} Either Text Link.Term

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run storeFunctorSearch

  Right (termLink #30iqh0fhqm)
```

## Alias the derived instance, mark it `given`, and use it

The `Link.Term` returned above points at the derived Functor instance
sitting in the codebase under its hash. Alias it to a name so we can
inspect it, mark it `given` so it participates in `Functor f`
resolution, and exercise it on a nested `Search` value.

``` ucm
scratch/main> alias.term #30iqh0fhqm Functor.search

  Done.

scratch/main> view Functor.search

  Functor.search : Functor Search
  Functor.search =
    Functor
      let
        myMap f = cases
          Found x0 -> Found (f x0)
          Continue x0 ->
            Continue (fmapWith optional (myMap f) x0)
        myMap

scratch/main> mark.given Functor.search

  Marked Functor.search. It will now participate in implicit
  resolution.
```

``` unison
sampleSearch : Search Nat
sampleSearch =
  Search.Continue (Some (Search.Continue (Some (Search.Found 41))))

showSearch : Search Nat -> Text
showSearch s = match s with
  Search.Found n -> "Found " ++ Nat.toText n
  Search.Continue None -> "Continue None"
  Search.Continue (Some inner) -> "Continue (Some " ++ showSearch inner ++ ")"

incSearch : '{IO, Exception} Text
incSearch _ =
  result = fmapWith Functor.search (n -> n Nat.+ 1) sampleSearch
  showSearch result
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + incSearch    : '{IO, Exception} Text
  + sampleSearch : Search Nat
  + showSearch   : Search Nat -> Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run incSearch

  "Continue (Some Continue (Some Found 42))"
```
