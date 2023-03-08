# Basics
## non-exhaustive patterns 
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
```unison
unique type T = A | B

test : (T, Optional T) -> ()
test = cases
  (A, Some _) -> ()
  (A, None) -> ()
  (B, Some A) -> ()
  (B, None) -> ()
```

```ucm

  Pattern match doesn't cover all possible cases:
        4 | test = cases
        5 |   (A, Some _) -> ()
        6 |   (A, None) -> ()
        7 |   (B, Some A) -> ()
        8 |   (B, None) -> ()
    
  
  Patterns not matched:
   * (B, Some B)

```
## redundant patterns
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
```unison
unique type T = A | B

test : (T, Optional T) -> ()
test = cases
  (A, Some _) -> ()
  (A, None) -> ()
  (B, Some _) -> ()
  (B, None) -> ()
  (A, Some A) -> ()
```

```ucm

  This case would be ignored because it's already covered by the preceding case(s):
        9 |   (A, Some A) -> ()
    

```
# Uninhabited patterns

match is complete without covering uninhabited patterns
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
uninhabited patterns are reported as redundant
```unison
unique type V =

test0 : V -> ()
test0 = cases
  _ -> ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type V
      test0 : V -> ()

```
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
# Guards

## Incomplete patterns due to guards should be reported
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
```unison
test : Optional Nat -> Nat
test = cases
  None -> 0
  Some x
    | isEven x -> x
```

```ucm

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   None -> 0
        4 |   Some x
        5 |     | isEven x -> x
    
  
  Patterns not matched:
   * Some _

```
## Complete patterns with guards should be accepted
```unison
test : Optional Nat -> Nat
test = cases
  None -> 0
  Some x
    | isEven x -> x
    | otherwise -> 0
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : Optional Nat -> Nat

```
# Pattern instantiation depth

Uncovered patterns are only instantiated as deeply as necessary to
distinguish them from existing patterns.
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

## Non-exhaustive

Nat
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
Boolean
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
## Exhaustive

Nat
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
Boolean
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
# Redundant

Nat
```unison
test : Nat -> ()
test = cases
  0 -> ()
  0 -> ()
  _ -> ()
```

```ucm

  This case would be ignored because it's already covered by the preceding case(s):
        4 |   0 -> ()
    

```
Boolean
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

## Exhaustive
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
## Non-exhaustive
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
   * (() +: _)

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
   * (() +: [])

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
   * (() +: (() +: _))

```
## Uninhabited

`Cons` is not expected since `V` is uninhabited
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
## Length restrictions can equate cons and nil patterns

Here the first pattern matches lists of length two or greater, the
second pattern matches lists of length 0. The third case matches when the
final element is `false`, while the fourth pattern matches when the
first element is `true`. However, the only possible list length at
the third or fourth clause is 1, so the first and final element must
be equal. Thus, the pattern match is exhaustive.
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
This is the same idea as above but shows that fourth match is redundant.
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
This is another similar example. The first pattern matches lists of
length 5 or greater. The second matches lists of length 4 or greater where the
first and third element are true. The third matches lists of length 4
or greater where the final 4 elements are `true, false, true, false`. 
The list must be exactly of length 4 to arrive at the second or third
clause, so the third pattern is redundant.
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
