Regression test for https://github.com/unisonweb/unison/pull/2377

``` ucm :hide
> builtins.merge
```

``` unison
structural ability Ab where
  a: Nat -> ()

dopey = cases
  ?0 -> ()
  _ -> ()

grumpy = cases
  d -> ()

happy = cases
  true -> ()
  false -> ()

sneezy = cases
  +1 -> ()
  _ -> ()

bashful = cases
  Some a -> ()
  _ -> ()

mouthy = cases
  [] -> ()
  _ -> ()

pokey = cases
  h +: t -> ()
  _ -> ()

sleepy = cases
  i :+ l -> ()
  _ -> ()

demure = cases
  [0] -> ()
  _ -> ()

angry = cases
  a ++ [] -> ()

tremulous = cases
  (0,1) -> ()
  _ -> ()

throaty = cases
  { Ab.a a -> k } -> ()
  { _ } -> ()

agitated = cases
  a | a == 2 -> ()
  _ -> ()

doc = cases
  y@4 -> ()
  _ -> ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural ability Ab

  + agitated  : Nat -> ()
  + angry     : [t] -> ()
  + bashful   : Optional a -> ()
  + demure    : [Nat] -> ()
  + doc       : Nat -> ()
  + dopey     : Char -> ()
  + grumpy    : ff284oqf651 -> ()
  + happy     : Boolean -> ()
  + mouthy    : [t] -> ()
  + pokey     : [t] -> ()
  + sleepy    : [t] -> ()
  + sneezy    : Int -> ()
  + throaty   : Request {g, Ab} x -> ()
  + tremulous : (Nat, Nat) -> ()

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view dopey

  dopey : Char -> ()
  dopey = cases
    ?0 -> ()
    _  -> ()

> view grumpy

  grumpy : ff284oqf651 -> ()
  grumpy = cases d -> ()

> view happy

  happy : Boolean -> ()
  happy = cases
    true  -> ()
    false -> ()

> view sneezy

  sneezy : Int -> ()
  sneezy = cases
    +1 -> ()
    _  -> ()

> view bashful

  bashful : Optional a -> ()
  bashful = cases
    Some a -> ()
    _      -> ()

> view mouthy

  mouthy : [t] -> ()
  mouthy = cases
    [] -> ()
    _  -> ()

> view pokey

  pokey : [t] -> ()
  pokey = cases
    h +: t -> ()
    _      -> ()

> view sleepy

  sleepy : [t] -> ()
  sleepy = cases
    i :+ l -> ()
    _      -> ()

> view demure

  demure : [Nat] -> ()
  demure = cases
    [0] -> ()
    _   -> ()

> view angry

  angry : [t] -> ()
  angry = cases a ++ [] -> ()

> view tremulous

  tremulous : (Nat, Nat) -> ()
  tremulous = cases
    (0, 1) -> ()
    _      -> ()

> view throaty

  throaty : Request {g, Ab} x -> ()
  throaty = cases
    { Ab.a a -> k } -> ()
    { _ }           -> ()

> view agitated

  agitated : Nat -> ()
  agitated = cases
    a | a == 2 -> ()
    _ -> ()

> view doc

  doc : Nat -> ()
  doc = cases
    y@4 -> ()
    _   -> ()
```
