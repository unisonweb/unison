Regression test for https://github.com/unisonweb/unison/pull/2377


```unison
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
  
agitated = cases
  a | a == 2 -> ()
  _ -> ()

doc = cases
  y@4 -> () 
  _ -> ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Ab
      agitated  : Nat -> ()
      angry     : [t] -> ()
      bashful   : Optional a -> ()
      demure    : [Nat] -> ()
      doc       : Nat -> ()
      dopey     : Char -> ()
      grumpy    : ff284oqf651 -> ()
      happy     : Boolean -> ()
      mouthy    : [t] -> ()
      pokey     : [t] -> ()
      sleepy    : [t] -> ()
      sneezy    : Int -> ()
      throaty   : Request {g, Ab} x -> ()
      tremulous : (Nat, Nat) -> ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural ability Ab
    agitated  : Nat -> ()
    angry     : [t] -> ()
    bashful   : Optional a -> ()
    demure    : [Nat] -> ()
    doc       : Nat -> ()
    dopey     : Char -> ()
    grumpy    : ff284oqf651 -> ()
    happy     : Boolean -> ()
    mouthy    : [t] -> ()
    pokey     : [t] -> ()
    sleepy    : [t] -> ()
    sneezy    : Int -> ()
    throaty   : Request {g, Ab} x -> ()
    tremulous : (Nat, Nat) -> ()

.> view dopey

  dopey : Char -> ()
  dopey = cases
    ?0 -> ()
    _  -> ()

.> view grumpy

  grumpy : ff284oqf651 -> ()
  grumpy = cases d -> ()

.> view happy

  happy : Boolean -> ()
  happy = cases
    true  -> ()
    false -> ()

.> view sneezy

  sneezy : Int -> ()
  sneezy = cases
    +1 -> ()
    _  -> ()

.> view bashful

  bashful : Optional a -> ()
  bashful = cases
    Some a -> ()
    _      -> ()

.> view mouthy

  mouthy : [t] -> ()
  mouthy = cases
    [] -> ()
    _  -> ()

.> view pokey

  pokey : [t] -> ()
  pokey = cases
    h +: t -> ()
    _      -> ()

.> view sleepy

  sleepy : [t] -> ()
  sleepy = cases
    i :+ l -> ()
    _      -> ()

.> view demure

  demure : [Nat] -> ()
  demure = cases
    [0] -> ()
    _   -> ()

.> view angry

  angry : [t] -> ()
  angry = cases a ++ [] -> ()

.> view tremulous

  tremulous : (Nat, Nat) -> ()
  tremulous = cases
    (0, 1) -> ()
    _      -> ()

.> view throaty

  throaty : Request {g, Ab} x -> ()
  throaty = cases {a a -> k} -> ()

.> view agitated

  agitated : Nat -> ()
  agitated = cases
    a | a == 2 -> ()
    _ -> ()

.> view doc

  doc : Nat -> ()
  doc = cases
    y@4 -> ()
    _   -> ()

```
