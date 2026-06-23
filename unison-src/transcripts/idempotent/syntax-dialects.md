# Pluggable surface syntax (dialects)

Unison stores code as a content-addressed AST, so the surface syntax is just a UI rendered on top of it. UCM can render
and parse that AST in several interchangeable *dialects*. Pick one with `syntax.set` (it persists in the codebase; the
`UNISON_SYNTAX` environment variable overrides it for quick experiments), and check the current one with `syntax.get`.

``` ucm :hide
scratch/main> builtins.mergeio
```

Let's define a few things using the default (`unison`) syntax: a function that uses operators, a record, and a data
type with a `match`.

``` unison
inc x = x + 1

combine : Nat -> Nat -> Nat -> Nat
combine a b c = a + b * c

-- a plain (non-infix) function call: applies `combine` and `inc` by name
demo : Nat -> Nat
demo n = combine (inc n) n 2

type Shape = Circle | Square | Triangle

unique type Point = { x : Nat, y : Nat }

isCircle s = match s with
  Circle -> true
  _ -> false
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Point
  + type Shape

  + combine        : Nat -> Nat -> Nat -> Nat
  + demo           : Nat -> Nat
  + inc            : Nat -> Nat
  + isCircle       : Shape -> Boolean
  + Point.x        : Point -> Nat
  + Point.x.modify : (Nat ->{g} Nat) -> Point ->{g} Point
  + Point.x.set    : Nat -> Point -> Point
  + Point.y        : Point -> Nat
  + Point.y.modify : (Nat ->{g} Nat) -> Point ->{g} Point
  + Point.y.set    : Nat -> Point -> Point

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

The current dialect is the default `unison` syntax:

``` ucm
scratch/main> syntax.get

  syntax.dialect is unset

scratch/main> view inc combine demo Shape Point isCircle

  type Point = { x : Nat, y : Nat }

  type Shape = Circle | Square | Triangle

  combine : Nat -> Nat -> Nat -> Nat
  combine a b c =
    use Nat * +
    a + b * c

  demo : Nat -> Nat
  demo n = combine (inc n) n 2

  inc : Nat -> Nat
  inc x =
    use Nat +
    x + 1

  isCircle : Shape -> Boolean
  isCircle = cases
    Circle -> true
    _      -> false
```

## Clojure-like S-expressions

``` ucm
scratch/main> syntax.set sexpr

scratch/main> view inc combine demo Shape Point isCircle

  (record Point ()
    (x Nat)
    (y Nat))

  (type Shape ()
    (Circle)
    (Square)
    (Triangle))

  (: combine (-> Nat Nat Nat Nat))
  (defn combine (a b c) (Nat.+ a (Nat.* b c)))

  (: demo (-> Nat Nat))
  (defn demo (n) (combine (inc n) n 2))

  (: inc (-> Nat Nat))
  (defn inc (x) (Nat.+ x 1))

  (: isCircle (-> Shape Boolean))
  (defn isCircle (s)
    (match s
      (case Circle true)
      (case _ false)))
```

The parser switches too — we can write S-expressions into the scratch file and add them:

``` unison
(def two (Nat.+ 1 1))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + two : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view two

  (: two Nat)
  (def two (Nat.+ 1 1))
```

## Curlison (curly-brace)

Note the minimal, precedence-driven parentheses in `combine`: `a Nat.+ b Nat.* c` (no parens around `b Nat.* c`).

``` ucm
scratch/main> syntax.set curlison

scratch/main> view inc combine demo Shape Point isCircle

  record Point {
    x : Nat;
    y : Nat;
  }

  type Shape {
    Circle;
    Square;
    Triangle;
  }

  Nat combine(Nat a, Nat b, Nat c) {
    return a Nat.+ b Nat.* c;
  }

  Nat demo(Nat n) {
    return combine(inc(n), n, 2);
  }

  Nat inc(Nat x) {
    return x Nat.+ 1;
  }

  Boolean isCircle(Shape s) {
    return match (s) {
      Circle => true;
      _ => false;
    };
  }
```

## Pyson (indentation-significant)

``` ucm
scratch/main> syntax.set pyson

scratch/main> view inc combine demo Shape Point isCircle

  record Point:
    x : Nat
    y : Nat

  type Shape:
    Circle
    Square
    Triangle

  combine : Nat -> Nat -> Nat -> Nat
  def combine(a, b, c):
    a Nat.+ b Nat.* c

  demo : Nat -> Nat
  def demo(n):
    combine(inc(n), n, 2)

  inc : Nat -> Nat
  def inc(x):
    x Nat.+ 1

  isCircle : Shape -> Boolean
  def isCircle(s):
    match s:
      case Circle:
        True
      case _:
        False
```

## BASIC-flavored (Basison)

``` ucm
scratch/main> syntax.set basison

scratch/main> view inc combine demo Shape Point isCircle

  RECORD Point
    x AS Nat
    y AS Nat
  END RECORD

  TYPE Shape
    Circle
    Square
    Triangle
  END TYPE

  combine AS Nat -> Nat -> Nat -> Nat
  FUNCTION combine(a, b, c)
    RETURN a Nat.+ b Nat.* c
  END FUNCTION

  demo AS Nat -> Nat
  FUNCTION demo(n)
    RETURN combine(inc(n), n, 2)
  END FUNCTION

  inc AS Nat -> Nat
  FUNCTION inc(x)
    RETURN x Nat.+ 1
  END FUNCTION

  isCircle AS Shape -> Boolean
  FUNCTION isCircle(s)
    RETURN SELECT CASE s
      CASE Circle:
        TRUE
      CASE _:
        FALSE
    END SELECT
  END FUNCTION
```

## Pascal/Ruby (Rubascal)

``` ucm
scratch/main> syntax.set rubascal

scratch/main> view inc combine demo Shape Point isCircle

  record Point
    x : Nat
    y : Nat
  end

  type Shape
    Circle
    Square
    Triangle
  end

  combine : Nat -> Nat -> Nat -> Nat
  def combine(a, b, c)
    a Nat.+ b Nat.* c
  end

  demo : Nat -> Nat
  def demo(n)
    combine(inc(n), n, 2)
  end

  inc : Nat -> Nat
  def inc(x)
    x Nat.+ 1
  end

  isCircle : Shape -> Boolean
  def isCircle(s)
    case s
      when Circle then
        true
      when _ then
        false
    end
  end
```

## Docs

A doc's prose is the same in every dialect, but the code embedded in it — inline examples and `@eval` blocks — is
rendered (and re-parsed) in the active dialect. Define a doc with a call to `inc` (back in the default syntax):

``` ucm
scratch/main> syntax.set unison
```

``` unison
incDoc : Doc2
incDoc = {{ Apply {inc} like `` inc 5 `` to get @eval{inc 5}. }}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + incDoc : Doc2

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

In the curly-brace dialect the embedded `inc 5` becomes `inc(5)`:

``` ucm
scratch/main> syntax.set curlison

scratch/main> view incDoc

  incDoc = {{ Apply {inc} like `` inc ( 5 ) `` to get @eval{inc(5)}. }};
```

In Pyson, likewise:

``` ucm
scratch/main> syntax.set pyson

scratch/main> view incDoc

  incDoc = {{ Apply {inc} like `` inc ( 5 ) `` to get @eval{inc(5)}. }}
```

## Back to the default syntax

``` ucm
scratch/main> syntax.set unison

scratch/main> view demo

  demo : Nat -> Nat
  demo n = combine (inc n) n 2
```
