
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
    bods = match name with
            "builtin-Char.toNat" -> "(char->integer x0)"
            "builtin-Char.fromNat" -> "(integer->char x0)"
            _ -> SchemeTerm.toIndentedText (n + 2) body
    pexpr
      (n + 2)
      [ "define-unison",
        parens (Text.join " " (name +: args)),
        bods ]

generateScheme : Boolean -> Link.Term ->{IO, Exception} Text
generateScheme exec base =
  found = intermediateCode base
  bref = #b64k0j6eu8 <| #i3vare757c base
  header =
    use Text ++
    "(top-level-program\n" ++ generateImports 2 mainImports ++ "\n; end toplevel\n"
  footer = if exec then executeFooter bref else compileFooter bref
  Text.join "\n\n; divider\n\n" [header, generateDefns false 2 found, footer]

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


scheme.generateDefns :
  Boolean -> Nat -> Map Link.Term SuperGroup ->{Exception} Text
scheme.generateDefns genIntermediate n found =
  use List map
  use Reference toScheme
  use Text ++ join
  hrefs =
    Map.foldLeft (rs -> Set.union rs << SuperGroup.handled) Set.empty found
  link tl = toScheme (fromTermLink tl)
  hdef r =
    err = "unhandled ability: " ++ toSchemeSym r
    errk =
      SExpr [Sym "lambda", SExpr [Sym "_"], SExpr [Sym "raise", String err]]
    SExpr [Sym "define", toScheme r, errk]
  hdefs = map (SchemeTerm.toIndentedText n << hdef) (Set.toList hrefs)
  defs =
    join (pad! n) (map (schemeOutput genIntermediate n) (Map.toList found))
  join (pad! n) hdefs ++ "\n; hdefs -> defs \n" ++ pad! n ++ defs

```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      printBytes : ∀ _. _ ->{IO, Exception} ()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      generateScheme                   : Boolean
                                         -> Link.Term
                                         ->{IO, Exception} Text
      scheme.SchemeDefn.toIndentedText : Nat
                                         -> SchemeDefn
                                         -> Text
      scheme.builtinLinks              : Map Text Link.Term
      scheme.generateDefns             : Boolean
                                         -> Nat
                                         -> Map
                                           Link.Term SuperGroup
                                         ->{Exception} Text
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
