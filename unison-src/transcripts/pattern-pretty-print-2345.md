Regression test for https://github.com/unisonweb/unison/pull/2377


```ucm:hide
.> builtins.merge
```

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
.> add
.> view dopey
.> view grumpy
.> view happy
.> view sneezy
.> view bashful
.> view mouthy
.> view pokey
.> view sleepy
.> view demure
.> view angry
.> view tremulous
.> view throaty
.> view agitated
.> view doc

```

