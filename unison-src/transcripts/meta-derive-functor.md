# Macro-derived `Functor` instance

A more interesting macro: derive `Functor` for `Optional`. The class
has a rank-N method (`forall a b. (a -> b) -> f a -> f b`), so the
type machinery has more to chew on than the `Show` case did.

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

## A user-supplied map for Optional

```unison
mapOptional : (a -> b) -> Optional a -> Optional b
mapOptional f = cases
  None -> None
  Some x -> Some (f x)
```

```ucm
scratch/main> add
```

## The macro

`deriveFunctor` takes any `forall a b. (a -> b) -> f a -> f b` function
and builds the AST of the matching `Functor f` instance. The
`${ Meta.decompile m }` splice lifts the runtime method into a
`meta.Term meta.TermF` reference; quasiquote handles the constructor
invocation.

```unison
deriveFunctor : (forall a b. (a -> b) -> f a -> f b) ->{IO} meta.Term meta.TermF
deriveFunctor m =
  methodTerm = Meta.decompile m
  [| Functor.Functor ${methodTerm} |]
```

```ucm
scratch/main> add
```

## Generate, store, and reify

```unison
storeFunctorOptional : '{IO} Either Text Link.Term
storeFunctorOptional _ =
  instance = !'(deriveFunctor mapOptional)
  Meta.store instance
```

```ucm
scratch/main> add
scratch/main> run storeFunctorOptional
```

## Alias and mark as given

(Hash substituted from the run output above.)

```ucm
scratch/main> alias.term #0b5med2gmp Functor.optional
scratch/main> view Functor.optional
scratch/main> mark.given Functor.optional
scratch/main> givens
```

## Use the instance

```unison
fmapOptional : (a -> b) -> Optional a -> Optional b
fmapOptional f x = match Functor.optional with
  Functor.Functor m -> m f x

example : '{IO} (Optional Nat, Optional Nat)
example _ =
  (fmapOptional (n -> n Nat.+ 1) (Some 41),
   fmapOptional (n -> n Nat.+ 1) None)
```

```ucm
scratch/main> add
scratch/main> run example
```
