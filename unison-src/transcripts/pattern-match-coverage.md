```ucm:hide
.> builtins.merge
```

# Basics
## non-exhaustive patterns 
```unison:error
unique type T = A | B | C

test : T -> ()
test = cases
  A -> ()
```

```unison:error
unique type T = A | B

test : (T, Optional T) -> ()
test = cases
  (A, Some _) -> ()
  (A, None) -> ()
  (B, Some A) -> ()
  (B, None) -> ()
```

## redundant patterns
```unison:error
unique type T = A | B | C

test : T -> ()
test = cases
  A -> ()
  B -> ()
  C -> ()
  _ -> ()
```

```unison:error
unique type T = A | B

test : (T, Optional T) -> ()
test = cases
  (A, Some _) -> ()
  (A, None) -> ()
  (B, Some _) -> ()
  (B, None) -> ()
  (A, Some A) -> ()
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

uninhabited patterns are reported as redundant
```unison:error
unique type V =

test0 : V -> ()
test0 = cases
  _ -> ()
```

```unison:error
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some _ -> ()
```

# Guards

## Incomplete patterns due to guards should be reported
```unison:error
test : () -> ()
test = cases
  () | false -> ()
```

```unison:error
test : Optional Nat -> Nat
test = cases
  None -> 0
  Some x
    | isEven x -> x
```

## Complete patterns with guards should be accepted
```unison:error
test : Optional Nat -> Nat
test = cases
  None -> 0
  Some x
    | isEven x -> x
    | otherwise -> 0
```

# Pattern instantiation depth

Uncovered patterns are only instantiated as deeply as necessary to
distinguish them from existing patterns.
```unison:error
unique type T = A | B | C

test : Optional (Optional T) -> ()
test = cases
  None -> ()
  Some None -> ()
```

```unison:error
unique type T = A | B | C

test : Optional (Optional T) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some (Some A) -> ()
```

# Literals

## Non-exhaustive

Nat
```unison:error
test : Nat -> ()
test = cases
  0 -> ()
```

Boolean
```unison:error
test : Boolean -> ()
test = cases
  true -> ()
```

## Exhaustive

Nat
```unison
test : Nat -> ()
test = cases
  0 -> ()
  _ -> ()
```

Boolean
```unison
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
```

# Redundant

Nat
```unison:error
test : Nat -> ()
test = cases
  0 -> ()
  0 -> ()
  _ -> ()
```

Boolean
```unison:error
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
  _ -> ()
```

# Sequences

## Exhaustive
```unison
test : [()] -> ()
test = cases
  [] -> ()
  x +: xs -> ()
```

## Non-exhaustive
```unison:error
test : [()] -> ()
test = cases
  [] -> ()
```

```unison:error
test : [()] -> ()
test = cases
  x +: xs -> ()
```

```unison:error
test : [()] -> ()
test = cases
  xs :+ x -> ()
```

```unison:error
test : [()] -> ()
test = cases
  x0 +: (x1 +: xs) -> ()
  [] -> ()
```

```unison:error
test : [()] -> ()
test = cases
  [] -> ()
  x0 +: [] -> ()
```

## Uninhabited

`Cons` is not expected since `V` is uninhabited
```unison
unique type V =

test : [V] -> ()
test = cases
  [] -> ()
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

This is the same idea as above but shows that fourth match is redundant.
```unison:error
test : [Boolean] -> ()
test = cases
  [a, b] ++ xs -> ()
  [] -> ()
  xs :+ true -> ()
  true +: xs -> ()
  _ -> ()
```

This is another similar example. The first pattern matches lists of
length 5 or greater. The second matches lists of length 4 or greater where the
first and third element are true. The third matches lists of length 4
or greater where the final 4 elements are `true, false, true, false`. 
The list must be exactly of length 4 to arrive at the second or third
clause, so the third pattern is redundant.
```unison:error
test : [Boolean] -> ()
test = cases
  [a, b, c, d, f] ++ xs -> ()
  [true, _, true, _] ++ _ -> ()
  _ ++ [true, false, true, false] -> ()
  _ -> ()
```
