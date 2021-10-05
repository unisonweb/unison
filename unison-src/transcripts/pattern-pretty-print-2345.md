Regression test for https://github.com/unisonweb/unison/pull/2377


```ucm:hide
.> builtins.merge
```

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

