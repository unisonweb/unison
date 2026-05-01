# Check outputs produced during fuzzy name resolution failures

## Setup

``` ucm :hide
> builtins.merge
```

## When given a term with the right name and the right type

### Then I successfully typecheck the term and allow it to be added

``` unison
myFunction : Float -> Int
myFunction = truncate
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + myFunction : Float -> Int

  Run `update` to apply these changes to your codebase.
```

## When given a term with the right name but wrong type

### Then I get a type mismatch error

``` unison :error
-- TODO: Can we exercise suggestions for right name wrong type without encountering type mismatch errors?
myFunction : Float -> Nat
myFunction = truncate
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found a value  of type:  Float -> Int
  where I expected to find:  Float ->{𝕖} Nat

      2 | myFunction : Float -> Nat
      3 | myFunction = truncate
```

## When given a term that matches with terms with similar names and the right type

### Then I get a name resolution error with suggestions to use one of the similar names

``` unison :error
-- This should be Float -> Float but it seems the stdlib is old
-- should match only truncate
myFunction : Float -> Int
myFunction = TRuncate
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what TRuncate refers to here:

      4 | myFunction = TRuncate

  I think its type should be:

      Float -> Int

  I found some terms in scope with similar names but different 
  types. Was any of these what you wanted?

  truncate0 : Int -> Nat
  truncate : Float -> Int
```

``` unison :error
-- Works with fully-qualified names
-- should match only builtin.io2.Ref.cas
myFunction : Ref {IO} a1 -> Ticket a1 -> a1 ->{IO} Boolean
myFunction = builtin.io2.Ref.ca1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what builtin.io2.Ref.ca1 refers to here:

      4 | myFunction = builtin.io2.Ref.ca1

  I think its type should be:

      Ref {IO} a1 -> Ticket a1 -> a1 ->{IO} Boolean

  I found a term in scope with a similar name but a different 
  type. Was this what you wanted?

  cas : Ref {IO} a -> Ticket a -> a ->{IO} Boolean
```

``` unison :hide
-- Works with definitions added to the code base interactively
byz.byte : Boolean
byz.byte = true

long.fully.qualified.name.foo.bar.biz.bite : Nat
long.fully.qualified.name.foo.bar.biz.bite = 123
```

``` ucm :hide
> add
```

``` unison :error
-- Long fully-qualified names are still suggested when in scope
x = biz.bete
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what biz.bete refers to here:

      2 | x = biz.bete

  its type could be anything.


  I found some terms in scope with similar names but different 
  types. Was any of these what you wanted?

  byte : Boolean
  bite : Nat
```

## When given a term that matches with terms with similar names but the wrong type

### Then I get a name resolution error with suggestions to use one of the similar names and adjust the type

``` unison :error
-- Should match truncate and truncate0 though they have different types
myFunction : Float -> Nat
myFunction = Flat.TRuncate2x4
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what Flat.TRuncate2x4 refers to here:

      3 | myFunction = Flat.TRuncate2x4

  I think its type should be:

      Float -> Nat

  I found some terms in scope with similar names but different 
  types. Was any of these what you wanted?

  truncate : Float -> Int
  truncate0 : Int -> Nat
```

``` unison :error
-- Works with short names
-- Should match short name * in Float/Int/Nat
myFunction : Int -> Int -> Boolean
myFunction = X
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what X refers to here:

      4 | myFunction = X

  I think its type should be:

      Int -> Int -> Boolean

  I found some terms in scope with similar names but different 
  types. Was any of these what you wanted?

  (Float.*) : Float -> Float -> Float
  (Int.*) : Int -> Int -> Int
  (Nat.*) : Nat -> Nat -> Nat
```

``` unison :error
-- Works with qualified short names
-- Should only match short names Int
myFunction : Int -> Int -> Boolean
myFunction = In.X
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what In.X refers to here:

      4 | myFunction = In.X

  I think its type should be:

      Int -> Int -> Boolean

  I found some terms in scope with similar names but different 
  types. Was any of these what you wanted?

  (Int.*) : Int -> Int -> Int
  (Int.+) : Int -> Int -> Int
  (Int.-) : Int -> Int -> Int
```

## When given a term that matches local terms not yet added to the code base

### Then I get a name resolution error with suggestions to use a similar name

``` unison :error
a : Boolean
a = 1 == 2

xyzlmno : Boolean
xyzlmno = 1 == 2

-- Should only match with local name a with the right type Boolean
f : ()
f =
  if A then xYzlmno else ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what A refers to here:

     10 |   if A then xYzlmno else ()

  I think its type should be:

      Boolean

  I found some terms in scope with similar names but different 
  types. Was any of these what you wanted?

  (Float.*) : Float -> Float -> Float
  (Int.*) : Int -> Int -> Int
  a : Boolean
```

``` unison :error
a : Boolean
a = 1 == 2

L.xyzlmno : Int
L.xyzlmno = +1

-- Should only match with local name L.xyzlmno although the type doesn't match
f : ()
f =
  if R.xYzlmno then A else ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what R.xYzlmno refers to here:

     10 |   if R.xYzlmno then A else ()

  I think its type should be:

      Boolean

  I found a term in scope with a similar name but a different 
  type. Was this what you wanted?

  L.xyzlmno : Int
```

## Notes

There is another case where the term matches with terms with the wrong name but the right type. This case was not added here because it is unclear how to produce a concrete example. This kind of suggestion does not seem to be produced in the code paths in Unison.TypeChecker
