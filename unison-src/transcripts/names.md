 Example uses of the `names` command and output
```ucm:hide
.> alias.type ##Int .builtins.Int
```

```unison:hide
type IntTriple = IntTriple (Int, Int, Int)
intTriple = IntTriple(+1, +1, +1)
```

```ucm:hide
.> add
```

```ucm
.> alias.type IntTriple namespc.another.TripleInt
.> alias.term intTriple namespc.another.tripleInt
.> names IntTriple
.> names intTriple
```