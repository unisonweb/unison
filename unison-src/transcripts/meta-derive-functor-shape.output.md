# Derive Functor from the shape of the data type

The previous Functor derive transcript still required the user to
supply the `map` implementation by hand. Now the macro pulls the
shape — the list of constructors and field arities — out of the
codebase via the new `Meta.dataDeclShape` builtin and *generates*
the map function for any positive ADT.

``` ucm :hide
scratch/main> builtins.mergeio
```

## The class

``` unison
unique type Functor f = Functor (forall a b. (a -> b) -> f a -> f b)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Functor f

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## meta.Term builders

Plain-Unison helpers for the pieces we need:

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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + emptySet       : Set Name
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
  + mkAbs          : Text -> meta.Term TermF -> meta.Term TermF
  + mkApp          : meta.Term TermF
                     -> meta.Term TermF
                     -> meta.Term TermF
  + mkConstructor  : ConstructorReference -> meta.Term TermF
  + mkLam          : Text -> meta.Term TermF -> meta.Term TermF
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

## The deriver

For each constructor `(ref, arity)` we emit a match case
`ref x0 x1 ... -> ref (f x0) (f x1) ...`. Wrapping the body with one
ABT.Abs per captured variable keeps the meta-encoded match in canonical
form.

``` unison
mkFunctorCase :
  (meta.ConstructorReference, Nat) -> meta.MatchCase (meta.Term meta.TermF)
mkFunctorCase tup = match tup with (cr, arity) ->
  ctorRef = match cr with
    meta.ConstructorReference.ConstructorReference r _ -> r
  ctorCid = match cr with
    meta.ConstructorReference.ConstructorReference _ cid -> cid
  pat = meta.Pattern.PConstructor ctorRef ctorCid
          (List.replicate arity meta.Pattern.PVar)
  varNames =
    List.map (n -> "x" ++ Nat.toText n) (List.range 0 arity)
  ctorTerm = mkConstructor cr
  step acc vn = mkApp acc (mkApp (mkVar "f") (mkVar vn))
  appliedToFArgs = List.foldl step ctorTerm varNames
  bodyWithAbs = List.foldRight mkAbs appliedToFArgs varNames
  meta.MatchCase.MatchCase pat None bodyWithAbs

-- Build the map function for a type whose declaration is at typeRef.
-- Returns None when the type isn't in the codebase.
deriveMap : meta.Reference ->{IO} Optional (meta.Term meta.TermF)
deriveMap typeRef =
  match Meta.dataDeclShape typeRef with
    None -> None
    Some ctors ->
      clauses = List.map mkFunctorCase ctors
      Some (mkLam "f" (mkLam "x" (mkMatch (mkVar "x") clauses)))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + deriveMap     : Reference ->{IO} Optional (meta.Term TermF)
  + mkFunctorCase : (ConstructorReference, Nat)
                    -> MatchCase (meta.Term TermF)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Get a type's meta.Reference from a sample value

Decompile any value of the type, walk into the constructor node, and
read off the type reference.

``` unison
typeRefOf : a ->{IO} Optional meta.Reference
typeRefOf sample = match Meta.decompile sample with
  meta.Term.Term _ abt -> match abt with
    meta.ABT.Tm tf -> match tf with
      meta.TermF.Constructor
        (meta.ConstructorReference.ConstructorReference r _) -> Some r
      meta.TermF.App
        (meta.Term.Term _ (meta.ABT.Tm
          (meta.TermF.Constructor
            (meta.ConstructorReference.ConstructorReference r _))))
        _ -> Some r
      _ -> None
    _ -> None
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + typeRefOf : a ->{IO} Optional Reference

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Derive Functor for Optional

``` unison
storeFunctorOptional : '{IO} Either Text Link.Term
storeFunctorOptional _ = match typeRefOf (Some 0) with
  None -> Left "couldn't get type ref"
  Some optionalRef -> match deriveMap optionalRef with
    None -> Left "Meta.dataDeclShape returned None"
    Some mapAst -> Meta.store [| Functor.Functor ${mapAst} |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + storeFunctorOptional : '{IO} Either Text Link.Term

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run storeFunctorOptional

  Right (termLink #d5p81hm53s)
```

## Alias, view, and use

``` ucm
scratch/main> alias.term #d5p81hm53s Functor.optional

  Done.

scratch/main> view Functor.optional

  Functor.optional : Functor Optional
  Functor.optional =
    Functor
      (f x -> (match x with
        Some x0 -> Some (f x0)
        None    -> None))

scratch/main> mark.given Functor.optional

  Marked Functor.optional. It will now participate in implicit
  resolution.
```

The view above shows the synthesised map function — note we never
wrote `cases None -> None | Some x -> Some (f x)`; the deriver built
it from the constructor list alone.

``` unison
example : '{IO} (Optional Nat, Optional Nat)
example _ = match Functor.optional with
  Functor.Functor m ->
    (m (n -> n Nat.+ 1) (Some 41), m (n -> n Nat.+ 1) None)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + example : '{IO} (Optional Nat, Optional Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> run example

  (Some 42, None)
```
