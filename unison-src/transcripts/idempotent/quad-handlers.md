This transcript checks cases for the warning about quadratic handlers.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
ability Ask a where
  ask : a

ability Tell a where
  tell : a -> ()

provide : a -> '{Ask a, g} r -> r
provide x k =
  h = cases
    { r } -> r
    { ask -> k } -> handle k x with h
  handle !k with h

forget1 : '{Tell a, Ask Nat} x -> x
forget1 k = handle provide 3 k with cases
  { r } -> r
  { tell _ -> k } -> forget1 k

forget2 : '{Tell a, Ask Nat} x -> x
forget2 k = handle provide 3 k with cases
  { r } -> r
  { tell _ -> k } -> forget2 do !k
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  🤔 I found some suspicious recursive ability handlers.

  The recursive occurrences of the handlers are called at a subset
  of the declared abilities, which might indicate that a separate
  handler is installed for each recursive call.

  The argument:

     17 |   { tell _ -> k } -> forget1 k

  only needs the abilities:

      {Tell a147}

  but the available abilities are:

        {Tell a147, Ask Nat}

  The argument:

     22 |   { tell _ -> k } -> forget2 do !k

  only needs the abilities:

      {Tell a84}

  but the available abilities are:

        {Tell a84, Ask Nat}

  To avoid this warning, you can give explicit types to the arguments
  of the recursive call to the handler.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ability Ask a
      ability Tell a
      forget1 : '{Ask Nat, Tell a} x -> x
      forget2 : '{Ask Nat, Tell a} x -> x
      provide : a -> '{g, Ask a} r ->{g} r
```
