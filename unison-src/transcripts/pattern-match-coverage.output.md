# Basics
non-exhaustive patterns are reported
```unison
unique type T = A | B | C

test : T -> ()
test = cases
  A -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        4 | test = cases
        5 |   A -> ()
    
  
  Patterns not matched:
  
    * B
    * C

```
redundant matches are reported
```unison
unique type T = A | B | C

test : T -> ()
test = cases
  A -> ()
  B -> ()
  C -> ()
  _ -> ()
```

```ucm

  This case would be ignored because it's already covered by the preceding case(s):
        8 |   _ -> ()
    

```
uninhabited patterns are not expected
```unison
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type V
      test : Optional (Optional V) -> ()

```
they are reported as redundant
```unison
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some _ -> ()
```

```ucm

  This case would be ignored because it's already covered by the preceding case(s):
        7 |   Some _ -> ()
    

```
boolean guards are considered
```unison
test : () -> ()
test = cases
  () | false -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   () | false -> ()
    
  
  Patterns not matched:
   * ()

```
uncovered patterns are only instantiated as deeply as necessary to
distinguish them from existing patterns
```unison
unique type T = A | B | C

test : Optional (Optional T) -> ()
test = cases
  None -> ()
  Some None -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        4 | test = cases
        5 |   None -> ()
        6 |   Some None -> ()
    
  
  Patterns not matched:
   * Some (Some _)

```
```unison
unique type T = A | B | C

test : Optional (Optional T) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some (Some A) -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        4 | test = cases
        5 |   None -> ()
        6 |   Some None -> ()
        7 |   Some (Some A) -> ()
    
  
  Patterns not matched:
  
    * Some (Some B)
    * Some (Some C)

```
# Literals
non-exhaustive nat
```unison
test : Nat -> ()
test = cases
  0 -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   0 -> ()
    
  
  Patterns not matched:
   * _

```
```unison
test : Nat -> ()
test = cases
  0 -> ()
  _ -> ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : Nat -> ()

```
non-exhaustive boolean
```unison
test : Boolean -> ()
test = cases
  true -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   true -> ()
    
  
  Patterns not matched:
   * false

```
```unison
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : Boolean -> ()

```
redundant boolean
```unison
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
  _ -> ()
```

```ucm

  This case would be ignored because it's already covered by the preceding case(s):
        5 |   _ -> ()
    

```
# Sequences
```unison
test : [()] -> ()
test = cases
  [] -> ()
  x +: xs -> ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : [()] -> ()

```
```unison
test : [()] -> ()
test = cases
  [] -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   [] -> ()
    
  
  Patterns not matched:
   * (_ +: _)

```
```unison
test : [()] -> ()
test = cases
  x +: xs -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   x +: xs -> ()
    
  
  Patterns not matched:
   * []

```
```unison
test : [()] -> ()
test = cases
  xs :+ x -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   xs :+ x -> ()
    
  
  Patterns not matched:
   * []

```
```unison
unique type V =

test : [V] -> ()
test = cases
  [] -> ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type V
      test : [V] -> ()

```
```unison
test : [()] -> ()
test = cases
  x0 +: (x1 +: xs) -> ()
  [] -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   x0 +: (x1 +: xs) -> ()
        4 |   [] -> ()
    
  
  Patterns not matched:
   * (_ +: [])

```
```unison
test : [()] -> ()
test = cases
  [] -> ()
  x0 +: [] -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   [] -> ()
        4 |   x0 +: [] -> ()
    
  
  Patterns not matched:
   * (_ +: (_ +: _))

```
cons and snoc patterns are equated when a length restriction implies
that they refer to the same element
```unison
test : [Boolean] -> ()
test = cases
  [a, b] ++ xs -> ()
  [] -> ()
  xs :+ false -> ()
  true +: xs -> ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : [Boolean] -> ()

```
```unison
test : [Boolean] -> ()
test = cases
  [a, b] ++ xs -> ()
  [] -> ()
  xs :+ true -> ()
  true +: xs -> ()
  _ -> ()
```

```ucm

  This case would be ignored because it's already covered by the preceding case(s):
        6 |   true +: xs -> ()
    

```
```unison
test : [Boolean] -> ()
test = cases
  [a, b, c, d, f] ++ xs -> ()
  [true, _, true, _] ++ _ -> ()
  _ ++ [true, false, true, false] -> ()
  _ -> ()
```

```ucm

  This case would be ignored because it's already covered by the preceding case(s):
        5 |   _ ++ [true, false, true, false] -> ()
    

```
