 Example uses of the `names` command and output
```unison
structural type IntTriple = IntTriple (Int, Int, Int)
intTriple = IntTriple(+1, +1, +1)
```

```ucm
.> alias.type IntTriple namespc.another.TripleInt

  Done.

.> alias.term intTriple namespc.another.tripleInt

  Done.

.> names IntTriple

  Type
  Hash:  #cp7a2qo5du
  Names: IntTriple namespc.another.TripleInt
  
  Term
  Hash:   #cp7a2qo5du#0
  Names:  IntTriple.IntTriple

.> names intTriple

  Term
  Hash:   #6nuu8h1ib1
  Names:  intTriple namespc.another.tripleInt

```
