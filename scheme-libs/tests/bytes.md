
Note: This should be forked off of the codebase created by base.md

```ucm
.> pull dolio.public.internal.trunk.impls impls
```

```unison
printBytes _ = printLine (base.Bytes.toHex (base.Bytes.fromList [100, 200, 16]))

-- removing Bytes.toList and Bytes.fromList
scheme.builtinLinks : Map Text Link.Term
scheme.builtinLinks =
  Map.fromList
    [ ("builtin-Bytes.empty", termLink #n21jacdb5b),
      ("builtin-Bytes.++", termLink #5o0nvn3t6l),
      ("builtin-Bytes.take", termLink #4kv8niiu94),
      ("builtin-Bytes.drop", termLink #80l7nu1u90),
      ("builtin-Bytes.at", termLink #7qjq1usqej),
      ("builtin-Bytes.size", termLink #t320fkv2d5),
      ("builtin-Bytes.flatten", termLink #cl56gpk7am) ]

```

```ucm:hide
.> update
.> add
.> run generateSchemeBuiltinLibrary
```

```unison
bytes = '(runInScheme 2 (termLink printBytes))
```

```ucm
.> run bytes
```

