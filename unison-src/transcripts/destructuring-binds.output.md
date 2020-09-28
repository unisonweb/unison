# Destructuring binds

Here's a couple examples:

```unison
ex1 : (a,b,(Nat,Nat)) -> Nat
ex1 tup =
  (a, b, (c,d)) = tup
  c + d
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ex1 : (a, b, (Nat, Nat)) -> Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    ex1 : (a, b, (Nat, Nat)) -> Nat

```
A destructuring bind is just syntax for a single branch pattern match. Notice that Unison detects this function as an alias of `ex1`:

```unison
ex2 : (a,b,(Nat,Nat)) -> Nat
ex2 tup = match tup with
  (a, b, (c,d)) -> c + d
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ex2 : (a, b, (Nat, Nat)) -> Nat
        (also named ex1)

```
Syntactically, the left-hand side of the bind can be any pattern and can even include guards, for instance, see below. Because a destructuring bind desugars to a regular pattern match, pattern match coverage will eventually cause this to not typecheck:

```unison
ex3 =
  Some x | x > 10 = Some 19
  x + 1
```

## Corner cases

Destructuring binds can't be recursive: the left-hand side bound variables aren't available on the right hand side. For instance, this doesn't typecheck:

```unison
ex4 =
  (a,b) = (a Nat.+ b, 19)
  "Doesn't typecheck"
```

```ucm

  I'm not sure what a means at line 2, columns 12-13
  
      2 |   (a,b) = (a Nat.+ b, 19)
  
  Whatever it is, it has a type that conforms to builtin.Nat.
  

```
Even though the parser accepts any pattern on the LHS of a bind, it looks pretty weird to see things like `12 = x`, so we avoid showing a destructuring bind when the LHS is a "literal" pattern (like `42` or "hi").

```unison
ex5 x = match x with
  12 -> "Hi"
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    ex5 : Nat -> Text

.> view ex5

  ex5 : Nat -> Text
  ex5 = cases 12 -> "Hi"

```
Notice how it prints as an ordinary match.

Also, for clarity, the pretty-printer shows a single-branch match if the match shadows free variables of the scrutinee, for example:

```unison
ex6 x = match x with
  (x, y) -> x Nat.+ y
```

For clarity, the pretty-printer leaves this alone, even though in theory it could be written `(x,y) = x; x + y`:

```ucm
.> add

  ⍟ I've added these definitions:
  
    ex6 : (Nat, Nat) -> Nat

.> view ex6

  ex6 : (Nat, Nat) -> Nat
  ex6 = cases
    (x, y) ->
      use Nat +
      x + y

```
