# Macro-derived `Functor` instance

A more interesting macro: derive `Functor` for `Optional`. The class
has a rank-N method (`forall a b. (a -> b) -> f a -> f b`), so the
type machinery has more to chew on than the `Show` case did.

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

## A user-supplied map for Optional

``` unison
mapOptional : (a -> b) -> Optional a -> Optional b
mapOptional f = cases
  None -> None
  Some x -> Some (f x)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + mapOptional : (a ->{g} b) -> Optional a ->{g} Optional b

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## The macro

`deriveFunctor` takes any `forall a b. (a -> b) -> f a -> f b` function
and builds the AST of the matching `Functor f` instance. The
`${ Meta.decompile m }` splice lifts the runtime method into a
`meta.Term meta.TermF` reference; quasiquote handles the constructor
invocation.

``` unison
deriveFunctor : (forall a b. (a -> b) -> f a -> f b) ->{IO} meta.Term meta.TermF
deriveFunctor m =
  methodTerm = Meta.decompile m
  [| Functor.Functor ${methodTerm} |]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + deriveFunctor : (∀ a b. (a -> b) -> f a -> f b)
                    ->{IO} meta.Term TermF

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Generate, store, and reify

``` unison
storeFunctorOptional : '{IO} Either Text Link.Term
storeFunctorOptional _ =
  instance = !'(deriveFunctor mapOptional)
  Meta.store instance
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

  Right (termLink #0b5med2gmp)
```

## Alias and mark as given

(Hash substituted from the run output above.)

``` ucm
scratch/main> alias.term #0b5med2gmp Functor.optional

  Done.

scratch/main> view Functor.optional

  Functor.optional : Functor Optional
  Functor.optional =
    Functor
      (f lvqq567c001 -> (match lvqq567c001 with
        None   -> None
        Some x -> Some (f x)))

scratch/main> mark.given Functor.optional

  Marked Functor.optional. It will now participate in implicit
  resolution.

scratch/main> givens

  Definitions marked as givens in the current namespace:

    Functor.optional
```

## Use the instance

``` unison
fmapOptional : (a -> b) -> Optional a -> Optional b
fmapOptional f x = match Functor.optional with
  Functor.Functor m -> m f x

example : '{IO} (Optional Nat, Optional Nat)
example _ =
  (fmapOptional (n -> n Nat.+ 1) (Some 41),
   fmapOptional (n -> n Nat.+ 1) None)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + example      : '{IO} (Optional Nat, Optional Nat)
  + fmapOptional : (a -> b) -> Optional a -> Optional b

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
