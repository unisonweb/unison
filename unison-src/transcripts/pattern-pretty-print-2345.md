Regression test for https://github.com/unisonweb/unison/pull/2377


```ucm:hide
scratch/main> builtins.merge
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
  { _ } -> ()
  
agitated = cases
  a | a == 2 -> ()
  _ -> ()

doc = cases
  y@4 -> () 
  _ -> ()
```

```ucm
scratch/main> add
scratch/main> view dopey
scratch/main> view grumpy
scratch/main> view happy
scratch/main> view sneezy
scratch/main> view bashful
scratch/main> view mouthy
scratch/main> view pokey
scratch/main> view sleepy
scratch/main> view demure
scratch/main> view angry
scratch/main> view tremulous
scratch/main> view throaty
scratch/main> view agitated
scratch/main> view doc

```

