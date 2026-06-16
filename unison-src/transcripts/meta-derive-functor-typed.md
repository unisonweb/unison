# Derive Functor with type-aware field dispatch

The previous `Meta.dataDeclShape` returned only the arity of each
constructor's field list, so the derived `map` applied `f` to every
field — fine for `Optional` (only one field, of type `a`) but wrong
for any constructor whose fields aren't all of the parameter type.

The updated `Meta.dataDeclShape` now returns
`[(meta.ConstructorReference, [meta.Term meta.TypeF])]` — one
constructor entry, with the *type* of each field rather than just the
count. The deriver consults each field type and only applies `f` when
the field has the form @Var name@ (i.e. is exactly the type's mapped
parameter); other fields pass through unchanged.

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
```

```ucm
scratch/main> add
```

## Type-aware deriver

`isVarType` checks whether a field type is bare `Var x` — meaning
exactly the parameter we're mapping over. Anything else (`Ref`, `App`,
`Arrow`, …) gets passed through unmodified.

```unison
isVarType : meta.Term meta.TypeF -> Boolean
isVarType ty = match ty with
  meta.Term.Term _ (meta.ABT.Var _) -> true
  _ -> false

mkFieldExpr : (Text, meta.Term meta.TypeF) -> meta.Term meta.TermF
mkFieldExpr tup = match tup with (varName, fieldType) ->
  if isVarType fieldType
  then mkApp (mkVar "f") (mkVar varName)
  else mkVar varName

mkFunctorCase :
  (meta.ConstructorReference, [meta.Term meta.TypeF])
  -> meta.MatchCase (meta.Term meta.TermF)
mkFunctorCase tup = match tup with (cr, fieldTypes) ->
  ctorRef = match cr with
    meta.ConstructorReference.ConstructorReference r _ -> r
  ctorCid = match cr with
    meta.ConstructorReference.ConstructorReference _ cid -> cid
  arity = List.size fieldTypes
  pat = meta.Pattern.PConstructor ctorRef ctorCid
          (List.replicate arity meta.Pattern.PVar)
  varNames =
    List.map (n -> "x" ++ Nat.toText n) (List.range 0 arity)
  -- For each (varName, fieldType), apply f only when the field is
  -- the mapped type variable.
  pairs = List.zip varNames fieldTypes
  argExprs = List.map mkFieldExpr pairs
  step acc arg = mkApp acc arg
  bodyApplied = List.foldl step (mkConstructor cr) argExprs
  bodyWithAbs = List.foldRight mkAbs bodyApplied varNames
  meta.MatchCase.MatchCase pat None bodyWithAbs

deriveMap : meta.Reference ->{IO} Optional (meta.Term meta.TermF)
deriveMap typeRef =
  match Meta.dataDeclShape typeRef with
    None -> None
    Some ctors ->
      clauses = List.map mkFunctorCase ctors
      Some (mkLam "f" (mkLam "x" (mkMatch (mkVar "x") clauses)))
```

```ucm
scratch/main> add
```

## Sample-driven type-reference lookup

```unison
-- Walk down the spine of applications until reaching the head; if
-- the head is a Constructor node, return its type reference.
typeRefOf : a ->{IO} Optional meta.Reference
typeRefOf sample = headRef (Meta.decompile sample)

headRef : meta.Term meta.TermF -> Optional meta.Reference
headRef tm = match tm with
  meta.Term.Term _ abt -> match abt with
    meta.ABT.Tm tf -> match tf with
      meta.TermF.Constructor
        (meta.ConstructorReference.ConstructorReference r _) -> Some r
      meta.TermF.App f _ -> headRef f
      _ -> None
    _ -> None
```

```ucm
scratch/main> add
```

## Test: Optional

```unison
storeFunctorOptional : '{IO} Either Text Link.Term
storeFunctorOptional _ = match typeRefOf (Some 0) with
  None -> Left "couldn't get type ref"
  Some optionalRef -> match deriveMap optionalRef with
    None -> Left "Meta.dataDeclShape returned None"
    Some mapAst -> Meta.store [| Functor.Functor ${mapAst} |]
```

```ucm
scratch/main> add
scratch/main> run storeFunctorOptional
```

## Test: a user-defined ADT with mixed fields

A `Tagged` type whose constructor stores a `Text` label AND an `a`.
With the old (untyped) shape API the deriver would (incorrectly)
apply `f` to the label. The new field-type check correctly leaves
the label alone and only `f`-applies the parameter-typed field.

```unison
unique type Tagged a = Tagged Text a
```

```ucm
scratch/main> add
```

```unison
storeFunctorTagged : '{IO} Either Text Link.Term
storeFunctorTagged _ = match typeRefOf (Tagged "x" 0) with
  None -> Left "couldn't get type ref"
  Some taggedRef -> match deriveMap taggedRef with
    None -> Left "Meta.dataDeclShape returned None"
    Some mapAst -> Meta.store [| Functor.Functor ${mapAst} |]
```

```ucm
scratch/main> add
scratch/main> run storeFunctorTagged
```

## Inspect the generated instances

```ucm
scratch/main> alias.term #d5p81hm53s Functor.optional
scratch/main> view Functor.optional
scratch/main> alias.term #g9a66blfs0 Functor.tagged
scratch/main> view Functor.tagged
```

`Functor.tagged`'s body leaves the `Text` label untouched and only
`f`-applies the parameter-typed field — exactly what the typed-shape
check buys us.

```unison
useTagged : '{IO} Tagged Nat
useTagged _ = match Functor.tagged with
  Functor.Functor m -> m (n -> n Nat.* 10) (Tagged "answer" 4)
```

```ucm
scratch/main> add
scratch/main> run useTagged
```
