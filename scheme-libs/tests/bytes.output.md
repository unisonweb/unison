
Note: This should be forked off of the codebase created by base.md

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

scheme.SchemeDefn.toIndentedText : Nat -> SchemeDefn -> Text
scheme.SchemeDefn.toIndentedText n = cases
  Define name args body ->
    use List +:
    use Nat +
    formattedBody = match name with
            "builtin-Char.toNat" -> "(char->integer x0)"
            "builtin-Char.fromNat" -> "(integer->char x0)"
            _ -> SchemeTerm.toIndentedText (n + 2) body
    pexpr
      (n + 2)
      [ "define-unison",
        parens (Text.join " " (name +: args)),
        formattedBody ]

schemeOutput : Boolean -> Nat -> (Link.Term, SuperGroup) ->{Exception} Text
schemeOutput genIntermediate n = cases
  (link, anf) ->
    use Text ++
    ref = fromTermLink link
    int =
      if genIntermediate then
        SchemeIntermed.toIndentedText n (toSchemeObject ref anf) ++ pad! n
      else ""
    exe =
      Text.join
        (pad! n)
        (List.map (SchemeDefn.toIndentedText n) (SuperGroup.toScheme ref anf))
    int ++ exe

```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      printBytes : ∀ _. _ ->{IO, Exception} ()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      scheme.SchemeDefn.toIndentedText : Nat
                                         -> SchemeDefn
                                         -> Text
      scheme.builtinLinks              : Map Text Link.Term
      schemeOutput                     : Boolean
                                         -> Nat
                                         -> ( Link.Term,
                                           SuperGroup)
                                         ->{Exception} Text

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

  "480\n"

```
