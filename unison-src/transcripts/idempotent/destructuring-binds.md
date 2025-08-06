# Destructuring binds

``` ucm :hide
> builtins.merge
```

Here's a couple examples:

``` unison
ex0 : Nat -> Nat
ex0 n =
  (a, _, (c,d)) = ("uno", "dos", (n, 7))
  c + d

ex1 : (a,b,(Nat,Nat)) -> Nat
ex1 tup =
  (a, b, (c,d)) = tup
  c + d
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ex0 : Nat -> Nat
  + ex1 : (a, b, (Nat, Nat)) -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view ex0 ex1

  ex0 : Nat -> Nat
  ex0 n =
    use Nat +
    (a, _, (c, d)) = ("uno", "dos", (n, 7))
    c + d

  ex1 : (a, b, (Nat, Nat)) -> Nat
  ex1 = cases (a, b, (c, d)) -> c Nat.+ d
```

Notice that `ex0` is printed using the `cases` syntax (but `ex1` is not). The pretty-printer currently prefers the `cases` syntax if definition can be printed using either destructuring bind or `cases`.

A destructuring bind is just syntax for a single branch pattern match. Notice that Unison detects this function as an alias of `ex1`:

``` unison
ex2 : (a,b,(Nat,Nat)) -> Nat
ex2 tup = match tup with
  (a, b, (c,d)) -> c + d
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ex2 : (a, b, (Nat, Nat)) -> Nat
      (also named ex1)

  Run `update` to apply these changes to your codebase.
```

## Corner cases

Destructuring binds can't be recursive: the left-hand side bound variables aren't available on the right hand side. For instance, this doesn't typecheck:

``` unison :error
ex4 =
  (a,b) = (a Nat.+ b, 19)
  "Doesn't typecheck"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what a refers to here:

      2 |   (a,b) = (a Nat.+ b, 19)

  I found some terms in scope with similar names but different 
  types. Was any of these what you wanted?

  (Float.*) : Float -> Float -> Float
  (Int.*) : Int -> Int -> Int
  (Nat.*) : Nat -> Nat -> Nat
```

Even though the parser accepts any pattern on the LHS of a bind, it looks pretty weird to see things like `12 = x`, so we avoid showing a destructuring bind when the LHS is a "literal" pattern (like `42` or "hi"). Again these examples wouldn't compile with coverage checking.

``` unison
ex5 : 'Text
ex5 _ = match 99 + 1 with
  12 -> "Hi"
  _ -> "Bye"

ex5a : 'Text
ex5a _ = match (99 + 1, "hi") with
  (x, "hi") -> "Not printed as a destructuring bind."
  _ -> "impossible"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ex5  : 'Text
  + ex5a : 'Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view ex5 ex5a

  ex5 : 'Text
  ex5 _ = match 99 Nat.+ 1 with
    12 -> "Hi"
    _  -> "Bye"

  ex5a : 'Text
  ex5a _ = match (99 Nat.+ 1, "hi") with
    (x, "hi") -> "Not printed as a destructuring bind."
    _         -> "impossible"
```

Notice how it prints both an ordinary match.

Also, for clarity, the pretty-printer shows a single-branch match if the match shadows free variables of the scrutinee, for example:

``` unison :hide
ex6 x = match x with
  (x, y) -> x Nat.+ y
```

For clarity, the pretty-printer leaves this alone, even though in theory it could be written `(x,y) = x; x + y`:

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view ex6

  ex6 : (Nat, Nat) -> Nat
  ex6 = cases (x, y) -> x Nat.+ y
```
