``` unison :error
type Exists f = Exists (forall r. (forall a. f a -> r) ->  r)

Exists.apply: (forall a. f a -> r) -> Exists f -> r
Exists.apply f = cases Exists run -> run f

type Dust =

type Attic a = {
  fall: a -> Dust
}

droop: Exists Attic -> (a -> Dust)
droop attic a =
  Exists.apply fall attic
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Attic a1 -> a1 -> Dust
  where I expected to find:  Attic a1 -> a -> 𝕣

      9 |   fall: a -> Dust
     10 | }
     11 | 
     12 | droop: Exists Attic -> (a -> Dust)
     13 | droop attic a =
     14 |   Exists.apply fall attic

    from right here:

      9 |   fall: a -> Dust
```
