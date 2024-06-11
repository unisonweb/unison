The `alias.many` command can be used to copy definitions from the current namespace into your curated one.
The names that will be used in the target namespace are the names you specify, relative to the current namespace:

```scratch
/main> help alias.many

  alias.many (or copy)
  `alias.many <relative1> [relative2...] <namespace>` creates aliases `relative1`, `relative2`, ...
  in the namespace `namespace`.
  `alias.many foo.foo bar.bar .quux` creates aliases `.quux.foo.foo` and `.quux.bar.bar`.

```

Let's try it!

```ucm
scratch/main> alias.many stuff.List.adjacentPairs stuff.List.all stuff.List.any stuff.List.chunk stuff.List.chunksOf stuff.List.dropWhile stuff.List.first stuff.List.init stuff.List.intersperse stuff.List.isEmpty stuff.List.last stuff.List.replicate stuff.List.splitAt stuff.List.tail stuff.List.takeWhile .mylib

  Nothing changed in .mylib .

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    stuff.List.adjacentPairs
    stuff.List.all
    stuff.List.any
    stuff.List.chunk
    stuff.List.chunksOf
    stuff.List.dropWhile
    stuff.List.first
    stuff.List.init
    stuff.List.intersperse
    stuff.List.isEmpty
    stuff.List.last
    stuff.List.replicate
    stuff.List.splitAt
    stuff.List.tail
    stuff.List.takeWhile

```

```ucm
scratch/main> alias.many stuff.List.adjacentPairs stuff.List.all stuff.List.any stuff.List.chunk stuff.List.chunksOf stuff.List.dropWhile stuff.List.first stuff.List.init stuff.List.intersperse stuff.List.isEmpty stuff.List.last stuff.List.replicate stuff.List.splitAt stuff.List.tail stuff.List.takeWhile .mylibscratch/main> find-in mylib
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    stuff.List.adjacentPairs
    stuff.List.all
    stuff.List.any
    stuff.List.chunk
    stuff.List.chunksOf
    stuff.List.dropWhile
    stuff.List.first
    stuff.List.init
    stuff.List.intersperse
    stuff.List.isEmpty
    stuff.List.last
    stuff.List.replicate
    stuff.List.splitAt
    stuff.List.tail
    stuff.List.takeWhile

