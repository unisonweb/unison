
Note: This should be forked off of the codebase created by base.md

```ucm
.> pull dolio.public.internal.trunk.impls impls

  ✅
  
  ✅ Successfully pulled into newly created namespace impls.

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

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      printBytes : ∀ _. _ ->{IO, Exception} ()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      scheme.builtinLinks : Map Text Link.Term

```
```unison
bytes = '(runInScheme 2 (termLink printBytes))
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bytes : '{IO, Exception} Text

```
```ucm
.> run bytes

  "Non-zero exit code! 255\nException in fx+: #\\0 is not a fixnum\n"

```
