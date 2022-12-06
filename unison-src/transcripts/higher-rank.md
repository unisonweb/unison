
This transcript does some testing of higher-rank types. Regression tests related to higher-rank types can be added here.

```ucm:hide
.> alias.type ##Nat Nat
.> alias.type ##Text Text
.> alias.type ##IO IO
```

In this example, a higher-rank function is defined, `f`. No annotation is needed at the call-site of `f`, because the lambda is being checked against the polymorphic type `forall a . a -> a`, rather than inferred:

```unison
f : (forall a . a -> a) -> (Nat, Text)
f id = (id 1, id "hi")

> f (x -> x)
```

Another example, involving abilities. Here the ability-polymorphic function is instantiated with two different ability lists, `{}` and `{IO}`:

```unison
f : (forall a g . '{g} a -> '{g} a) -> () -> () 
f id _ = 
  _ = (id ('1 : '{} Nat), id ('("hi") : '{IO} Text))
  ()
```

Here's an example, showing that polymorphic functions can be fields of a constructor, and the functions remain polymorphic even when the field is bound to a name during pattern matching:

```unison
unique type Functor f = Functor (forall a b . (a -> b) -> f a -> f b)

Functor.map : Functor f -> (forall a b . (a -> b) -> f a -> f b)
Functor.map = cases Functor f -> f

Functor.blah : Functor f -> ()
Functor.blah = cases Functor f -> 
  g : forall a b . (a -> b) -> f a -> f b
  g = f
  ()
```

This example is similar, but involves abilities: 

```unison
unique ability Remote t where doRemoteStuff : ()
unique type Loc = Loc (forall t a . '{Remote t} a ->{Remote t} t a) 

Loc.blah : Loc -> ()
Loc.blah = cases Loc f ->
  f0 : '{Remote tx} ax ->{Remote tx} tx ax
  f0 = f
  ()

-- In this case, no annotation is needed since the lambda 
-- is checked against a polymorphic type
Loc.transform : (forall t a . '{Remote t} a -> '{Remote t} a) 
             -> Loc -> Loc
Loc.transform nt = cases Loc f -> Loc (a -> f (nt a))

-- In this case, the annotation is needed since f' is inferred
-- on its own it won't infer the higher-rank type
Loc.transform2 : (forall t a . '{Remote t} a -> '{Remote t} a) 
             -> Loc -> Loc
Loc.transform2 nt = cases Loc f -> 
  f' : forall t a . '{Remote t} a ->{Remote t} t a
  f' a = f (nt a)
  Loc f' 
```

## Types with polymorphic fields

```unison:hide
structural type HigherRanked = HigherRanked (forall a. a -> a)
```

We should be able to add and view records with higher-rank fields.

```ucm
.higher_ranked> add
.higher_ranked> view HigherRanked
```
