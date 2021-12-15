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
  Hash:  #ap1scd256n
  Names: IntTriple namespc.another.TripleInt
  
  Term
  Hash:   #ap1scd256n#0
  Names:  IntTriple.IntTriple

.> names intTriple

  Term
  Hash:   #rliag116kp
  Names:  intTriple namespc.another.tripleInt

```
