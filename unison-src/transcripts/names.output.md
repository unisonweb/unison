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
  Hash:  #p1iakck1ol
  Names: IntTriple namespc.another.TripleInt
  
  Term
  Hash:   #p1iakck1ol#0
  Names:  IntTriple.IntTriple

.> names intTriple

  Term
  Hash:   #2quul9e9bo
  Names:  intTriple namespc.another.tripleInt

```
