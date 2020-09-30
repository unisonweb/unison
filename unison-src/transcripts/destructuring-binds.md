# Destructuring binds

```ucm:hide
.> builtins.merge
```

Here's a couple examples:

```unison
ex1 : (a,b,(Nat,Nat)) -> Nat
ex1 tup =
  (a, b, (c,d)) = tup
  c + d
```

```ucm
.> add
.> view ex1
```

A destructuring bind is just syntax for a single branch pattern match. Notice that Unison detects this function as an alias of `ex1`:

```unison
ex2 : (a,b,(Nat,Nat)) -> Nat
ex2 tup = match tup with
  (a, b, (c,d)) -> c + d
```

Syntactically, the left-hand side of the bind can be any pattern and can even include guards, for instance, see below. Because a destructuring bind desugars to a regular pattern match, pattern match coverage will eventually cause this to not typecheck:

```unison:hide
ex3 =
  Some x | x > 10 = Some 19
  x + 1
```

## Corner cases

Destructuring binds can't be recursive: the left-hand side bound variables aren't available on the right hand side. For instance, this doesn't typecheck:

```unison:error
ex4 =
  (a,b) = (a Nat.+ b, 19)
  "Doesn't typecheck"
```

Even though the parser accepts any pattern on the LHS of a bind, it looks pretty weird to see things like `12 = x`, so we avoid showing a destructuring bind when the LHS is a "literal" pattern (like `42` or "hi").

```unison:hide
ex5 x = match x with
  12 -> "Hi"
```

```ucm
.> add
.> view ex5
```

Notice how it prints as an ordinary match.

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
