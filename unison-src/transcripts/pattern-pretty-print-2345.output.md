Regression test for https://github.com/unisonweb/unison/pull/2377


```unison
structural ability Ab where
  a: Nat -> ()

dopey = cases
  ?0 -> ()

grumpy = cases
  d -> ()

happy = cases
  true -> ()

sneezy = cases
  +1 -> ()

bashful = cases
  Some a -> ()

mouthy = cases
  [] -> ()

pokey = cases
  h +: t -> ()

sleepy = cases
  i :+ l -> ()

demure = cases
  [0] -> ()

angry = cases
  a ++ [] -> ()

tremulous = cases
  (0,1) -> ()

throaty = cases
  { Ab.a a -> k } -> ()
  
agitated = cases
  a | a == 2 -> ()

doc = cases
  y@4 -> () 
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
      grumpy    : q5g5surm1d1 -> ()
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
    grumpy    : q5g5surm1d1 -> ()
    happy     : Boolean -> ()
    mouthy    : [t] -> ()
    pokey     : [t] -> ()
    sleepy    : [t] -> ()
    sneezy    : Int -> ()
    throaty   : Request {g, Ab} x -> ()
    tremulous : (Nat, Nat) -> ()

.> view dopey

  dopey : Char -> ()
  dopey = cases ?0 -> ()

.> view grumpy

  grumpy : q5g5surm1d1 -> ()
  grumpy = cases d -> ()

.> view happy

  happy : Boolean -> ()
  happy = cases true -> ()

.> view sneezy

  sneezy : Int -> ()
  sneezy = cases +1 -> ()

.> view bashful

  bashful : Optional a -> ()
  bashful = cases Some a -> ()

.> view mouthy

  mouthy : [t] -> ()
  mouthy = cases [] -> ()

.> view pokey

  pokey : [t] -> ()
  pokey = cases h +: t -> ()

.> view sleepy

  sleepy : [t] -> ()
  sleepy = cases i :+ l -> ()

.> view demure

  demure : [Nat] -> ()
  demure = cases [0] -> ()

.> view angry

  angry : [t] -> ()
  angry = cases a ++ [] -> ()

.> view tremulous

  tremulous : (Nat, Nat) -> ()
  tremulous = cases (0, 1) -> ()

.> view throaty

  throaty : Request {g, Ab} x -> ()
  throaty = cases {a a -> k} -> ()

.> view agitated

  agitated : Nat -> ()
  agitated = cases a | a == 2 -> ()

.> view doc

  doc : Nat -> ()
  doc = cases y@4 -> ()

```
