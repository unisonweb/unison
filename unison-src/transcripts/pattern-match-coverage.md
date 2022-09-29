```ucm:hide
.> builtins.merge
```

# Basics
non-exhaustive patterns are reported
```unison:error
unique type T = A | B | C

test : T -> ()
test = cases
  A -> 0
```

redundant matches are reported
```unison:error
unique type T = A | B | C

test : T -> ()
test = cases
  A -> 0
  B -> 0
  C -> 0
  _ -> 0
```

patterns that would imply supplying an uninhabited type are not expected
```unison
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
```

they are reported as redundant
```unison:error
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some _ -> ()
```

boolean guards are considered
```unison:error
test : () -> ()
test = cases
  () | false -> ()
```

uncovered patterns are only instantiated as deeply as necessary to
distinguish them from existing patterns
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
non-exhaustive nat
```unison:error
test : Nat -> ()
test = cases
  0 -> ()
```

```unison
test : Nat -> ()
test = cases
  0 -> ()
  _ -> ()
```

non-exhaustive boolean
```unison:error
test : Boolean -> ()
test = cases
  true -> ()
```

```unison
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
```

redundant boolean
```unison:error
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
  _ -> ()
```

# Sequences
```unison
test : [()] -> ()
test = cases
  [] -> ()
  x +: xs -> ()
```

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

```unison
unique type V =

test : [V] -> ()
test = cases
  [] -> ()
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

```unison:error
test : [Boolean] -> ()
test = cases
  [a, b] ++ xs -> ()
  [] -> ()
  xs :+ true -> ()
  true +: xs -> ()
  _ -> ()
```

```unison:error
test : [Boolean] -> ()
test = cases
  [a, b, c, d, f] ++ xs -> ()
  [true, _, true, _] ++ _ -> ()
  _ ++ [true, false, true, false] -> ()
  _ -> ()
```
