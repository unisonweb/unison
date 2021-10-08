# Resolution Errors

This transcript tests the errors printed to the user when a name cannot be resolved.

## Codebase Setup

First we define differing types with the same name in different namespaces:

```ucm
.> cd example.resolution_failures

  ☝️  The namespace .example.resolution_failures is empty.

```
Now let's add a term named `a.foo`:

```unison
unique type one.AmbiguousType = one.AmbiguousType
unique type two.AmbiguousType = two.AmbiguousType

one.ambiguousTextTerm = "term one"
two.ambiguousTextTerm = "term two"

unique type A = One | Two
unique type B = B

one.ambiguousTermWithDifferingTypes = One
two.ambiguousTermWithDifferingTypes = Two
three.ambiguousTermWithDifferingTypes = B
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type A
      unique type B
      unique type one.AmbiguousType
      unique type two.AmbiguousType
      one.ambiguousTermWithDifferingTypes   : A
      one.ambiguousTextTerm                 : ##Text
      three.ambiguousTermWithDifferingTypes : B
      two.ambiguousTermWithDifferingTypes   : A
      two.ambiguousTextTerm                 : ##Text

```
```ucm
.example.resolution_failures> add

  ⍟ I've added these definitions:
  
    unique type A
    unique type B
    unique type one.AmbiguousType
    unique type two.AmbiguousType
    one.ambiguousTermWithDifferingTypes   : A
    one.ambiguousTextTerm                 : ##Text
    three.ambiguousTermWithDifferingTypes : B
    two.ambiguousTermWithDifferingTypes   : A
    two.ambiguousTextTerm                 : ##Text

```
## Tests

Now we introduce code which isn't sufficiently qualified. 
It is ambiguous which type from which namespace we mean.

We expect the output to:

1. Print all ambiguous usage sites separately
2. Print possible disambiguation suggestions for each unique ambiguity

```unison
-- We intentionally avoid using a constructor to ensure the constructor doesn't
-- affect type resolution.
useAmbiguousType : AmbiguousType -> ()
useAmbiguousType _ = ()

useUnknownType : UnknownType -> ()
useUnknownType _ = ()

-- Despite being a duplicate disambiguation, this should still be included in the annotations printout
separateAmbiguousTypeUsage : AmbiguousType -> ()
separateAmbiguousTypeUsage _ = ()
```

```ucm

  
    ❓
    
    I couldn't resolve any of these symbols:
    
        3 | useAmbiguousType : AmbiguousType -> ()
        4 | useAmbiguousType _ = ()
        5 | 
        6 | useUnknownType : UnknownType -> ()
        7 | useUnknownType _ = ()
        8 | 
        9 | -- Despite being a duplicate disambiguation, this should still be included in the annotations printout
       10 | separateAmbiguousTypeUsage : AmbiguousType -> ()
    
    
    Symbol          Suggestions         
                                        
    AmbiguousType   one.AmbiguousType   
                    two.AmbiguousType   
                                        
    UnknownType     No matches          
  

```
Currently, ambiguous terms are caught and handled by type directed name resolution,
but expect it to eventually be handled by the above machinery.

```unison
useAmbiguousTextTermNoTypeHint = ambiguousTextTerm
```

```ucm

  I'm not sure what ambiguousTextTerm means at line 1, columns 34-51
  
      1 | useAmbiguousTextTermNoTypeHint = ambiguousTextTerm
  
  There are no constraints on its type.
  
  Symbol              Suggestions             Type
                                              
  ambiguousTextTerm   one.ambiguousTextTerm   ##Text
                      two.ambiguousTextTerm   ##Text

```
```unison
useAmbiguousTermWithDifferingTypes = ambiguousTermWithDifferingTypes
```

```ucm

  I'm not sure what ambiguousTermWithDifferingTypes means at line 1, columns 38-69
  
      1 | useAmbiguousTermWithDifferingTypes = ambiguousTermWithDifferingTypes
  
  There are no constraints on its type.
  
  Symbol                            Suggestions                             Type
                                                                            
  ambiguousTermWithDifferingTypes   one.ambiguousTermWithDifferingTypes     A
                                    three.ambiguousTermWithDifferingTypes   B
                                    two.ambiguousTermWithDifferingTypes     A

```
```unison
useAmbiguousTermWithDifferingTypesWithTypeHint : A
useAmbiguousTermWithDifferingTypesWithTypeHint 
  = ambiguousTermWithDifferingTypes
```

```ucm

  I'm not sure what ambiguousTermWithDifferingTypes means at line 3, columns 5-36
  
      3 |   = ambiguousTermWithDifferingTypes
  
  Whatever it is, it has a type that conforms to A.
  
  Symbol                            Suggestions                           Type
                                                                          
  ambiguousTermWithDifferingTypes   one.ambiguousTermWithDifferingTypes   A
                                    two.ambiguousTermWithDifferingTypes   A

```
