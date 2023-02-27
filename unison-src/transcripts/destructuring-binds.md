# Destructuring binds

```ucm:hide
.> builtins.merge
```

Here's a couple examples:

```unison
ex0 : Nat -> Nat
ex0 n =
  (a, _, (c,d)) = ("uno", "dos", (n, 7))
  c + d

ex1 : (a,b,(Nat,Nat)) -> Nat
ex1 tup =
  (a, b, (c,d)) = tup
  c + d
```

```ucm
.> add
.> view ex0 ex1
```

Notice that `ex0` is printed using the `cases` syntax (but `ex1` is not). The pretty-printer currently prefers the `cases` syntax if definition can be printed using either destructuring bind or `cases`.

A destructuring bind is just syntax for a single branch pattern match. Notice that Unison detects this function as an alias of `ex1`:

```unison
ex2 : (a,b,(Nat,Nat)) -> Nat
ex2 tup = match tup with
  (a, b, (c,d)) -> c + d
```

## Corner cases

Destructuring binds can't be recursive: the left-hand side bound variables aren't available on the right hand side. For instance, this doesn't typecheck:

```unison:error
ex4 =
  (a,b) = (a Nat.+ b, 19)
  "Doesn't typecheck"
```

Even though the parser accepts any pattern on the LHS of a bind, it looks pretty weird to see things like `12 = x`, so we avoid showing a destructuring bind when the LHS is a "literal" pattern (like `42` or "hi"). Again these examples wouldn't compile with coverage checking.

```unison
ex5 : 'Text
ex5 _ = match 99 + 1 with
  12 -> "Hi"

ex5a : 'Text
ex5a _ = match (99 + 1, "hi") with
  (x, "hi") -> "Not printed as a destructuring bind."
```

```ucm
.> add
.> view ex5 ex5a
```

Notice how it prints both an ordinary match.

Also, for clarity, the pretty-printer shows a single-branch match if the match shadows free variables of the scrutinee, for example:

```unison:hide
ex6 x = match x with
  (x, y) -> x Nat.+ y
```

For clarity, the pretty-printer leaves this alone, even though in theory it could be written `(x,y) = x; x + y`:

```ucm
.> add
.> view ex6
```
