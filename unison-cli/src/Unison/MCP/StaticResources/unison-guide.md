USE THE FOLLOWING AS YOUR AUTHORITATIVE SOURCE OF INFORMATION ABOUT THE UNISON PROGRAMMING LANGUAGE.

# Unison Programming Language Guide

Unison is a statically typed functional language with a unique approach to handling effects and distributed computation. This guide focuses on the core language features, syntax, common patterns, and style conventions.

## Core Language Features

Unison is a statically typed functional programming language with typed effects (called "abilities" or algebraic effects). It uses strict evaluation (not lazy by default) and has proper tail calls for handling recursion.

## Function Syntax

Functions in Unison follow an Elm-style definition with type signatures on a separate line:

```
factorial : Nat -> Nat
factorial n = product (range 1 (n + 1))
```

## Binary operators

Binary operators are just functions written with infix syntax like `expr1 * expr2` or `expr1 Text.++ expr2`. They are just like any other, except that their unqualified name isn't alphanumeric operator, which tells the parser to parse them with infix syntax.

Any operator can also be written with prefix syntax. So `1 Nat.+ 1` can also be written as `(Nat.+) 1 1`.

The prefix syntax is also how to pass an operator to another function. For instance:

```
sum = List.foldLeft (Nat.+) 0
product = List.foldLeft (Nat.*) 1
dotProduct = Nat.sum (List.zipWith (*) [1,2,3] [4,5,6])
```

IMPORTANT: when passing an operator as an argument to a higher order function, surround it in parens, as above. Otherwise the parser will treat it as an infix expression.

### Currying and Multiple Arguments

Functions in Unison are automatically curried. A function type like `Nat -> Nat -> Nat` can be thought of as either:
- A function taking two natural numbers
- A function taking one natural number and returning a function of type `Nat -> Nat`

Arrow types (`->`) associate to the right. Partial application is supported by simply providing fewer arguments than the function expects:

```
add : Nat -> Nat -> Nat
add x y = x + y

add5 : Nat -> Nat
add5 = add 5  -- Returns a function that adds 5 to its argument
```

## Lambdas

Anonymous functions or lambdas are written like `x -> x + 1` or `x y -> x + y*2` with the arguments separated by spaces.

Here's an example:

```
List.zipWith (x y -> x*10 + y) [1,2,3] [4,5,6]
```

## Type Variables and Quantification

In Unison, lowercase variables in type signatures are implicitly universally quantified. You can also explicitly quantify variables using `forall`:

```
-- Implicit quantification
map : (a -> b) -> [a] -> [b]

-- Explicit quantification
map : forall a b . (a -> b) -> [a] -> [b]
```

## Algebraic Data Types

Unison uses algebraic data types similar to other functional languages:

```
type Optional a = None | Some a

type Either a b = Left a | Right b
```

Pattern matching works as you would expect:

```
Optional.map : (a -> b) -> Optional a -> Optional b
Optional.map f o = match o with
  None -> None
  Some a -> Some (f a)
```

#### Using `cases`

A function that immediately pattern matches on its last argument, like so:

```
Optional.map f o = match o with
  None -> None
  Some a -> Some (f a)
```

Can instead be written as:

```
Optional.map f = cases
  None -> None
  Some a -> Some (f a)
```

Prefer this style when applicable.

VERY IMPORTANT: the pattern matching syntax is different from Haskell or Elm. You cannot do pattern matching to the left of the `=` as you can in Haskell and Elm. ALWAYS introduce a pattern match using a `match <expr> with <cases>` form, or using the `cases` keyword.

INCORRECT (DO NOT DO THIS, IT IS INVALID):

```
List.head : [a] -> Optional a
List.head [] = None
List.head (hd +: _tl) = Some hd
```

CORRECT:

```
List.head : [a] -> Optional a
List.head = cases
  [] -> None
  hd +: _tl -> Some hd
```

ALSO CORRECT:

```
List.head : [a] -> Optional a
List.head as = match as with
  [] -> None
  hd +: _tl -> Some hd
```

### Important naming convention

Note that unlike Haskell or Elm, Unison's `Optional` type uses `None` and `Some` as constructors (not `Nothing` and `Just`). Becoming familiar with Unison's standard library naming conventions is important.

Use short variable names for helper functions. For instance: `rem` instead of `remainder`.

## Lists

Here is the syntax for a list:

```
[1,2,3]

-- append to the end of a list
[1,2,3] :+ 4 === [1,2,3,4]

-- prepend to the beginning of a list
0 +: [1,2,3] === [0,1,2,3]

[1,2,3] ++ [4,5,6] === [1,2,3,4,5,6]
```

List are represented as finger trees, so adding elements to the start or end is very fast, and random access using `List.at` is also fast.

IMPORTANT: DO NOT build up lists in reverse order, then call `List.reverse` at the end. Instead just build up lists in order, using `:+` to add to the end.

## Use accumulating parameters and tail recursive functions for looping

Tail recursion is the sole looping construct. Just write recursive functions, but write them tail recursive with an accumulating parameter. For instance, here is a function for summing a list:

```
Nat.sum : [Nat] -> Nat
Nat.sum ns =
  go acc = cases
    [] -> acc
    x +: xs -> go (acc + x) xs
  go 0 ns
```

IMPORTANT: If you're writing a function on a list, use tail recursion and an accumulating parameter.

CORRECT:

```
List.map : (a ->{g} b) -> [a] ->{g} [b]
List.map f as =
  go acc = cases
    [] -> acc
    x +: xs -> go (acc :+ f x) xs
  go [] as
```

INCORRECT:

```
-- DON'T DO THIS
List.map : (a ->{g} b) -> [a] ->{g} [b]
List.map f = cases
  [] -> []
  x +: xs -> f x +: go xs
```

## Built up lists in order, do not build them up in reverse order and reverse them at the end

Unison lists support O(1) append at the end of the list (they are based on finger trees), so you can just build up the list in order. Do not build them up in reverse order and then reverse at the end.

INCORRECT:

```
List.map : (a ->{g} b) -> [a] ->{g} [b]
List.map f as =
  go acc = cases
    [] -> List.reverse acc
    x +: xs -> go (f x +: acc) xs
  go [] as
```

CORRECT:

```
List.map : (a ->{g} b) -> [a] ->{g} [b]
List.map f as =
  go acc = cases
    [] -> acc
    x +: xs -> go (acc :+ f x) xs
  go [] as
```

Note that `:+` appends a value onto the end of a list in constant time, O(1):

```
[1,2,3] :+ 4
=== [1,2,3,4]
```

While `+:` prepends a value onto the beginning of a list, in constant time:

```
0 +: [1,2,3]
=== [0,1,2,3]
```

## Abilities (Algebraic Effects)

Unison has a typed effect system called "abilities" which allows you to specify what effects a function can perform:

```
-- A function with effects
Optional.map : (a ->{g} b) -> Optional a ->{g} Optional b
```

The `{g}` notation represents effects that the function may perform. Effects are propagated through the type system.

### Defining Abilities

You can define your own abilities:

```
ability Exception where
  raise : Failure -> x
```

An ability defines operations that functions can use. In this case, `Exception` has a single operation `raise` that takes a `Failure` and returns any type (allowing it to abort computation).

### Using Abilities

Built-in abilities like `IO` allow for side effects:

```
printLine : Text ->{IO, Exception} ()
```

The type signature shows that `printLine` can use both `IO` and `Exception` abilities.

### Handling Abilities

Ability handlers interpret the operations of an ability:

```
Exception.toEither : '{g, Exception} a ->{g} Either Failure a
Exception.toEither a =
  handle a()
  with cases
    { a } -> Right a
    { Exception.raise f -> resume } -> Left f
```

Handlers can transform one ability into another or eliminate them entirely.

### Ability Handler Style Guidelines

When implementing ability handlers, follow these style guidelines:

1. Use `go` or `loop` as the conventional name for recursive helper functions in handlers
2. Keep handler state as function arguments rather than using mutable state
3. For recursive handlers that resume continuations, structure them like this:

```
Stream.map : (a ->{g} b) -> '{Stream a, g} () -> '{Stream b, g} ()
Stream.map f sa = do
  go = cases
    { () } -> ()
    { Stream.emit a -> resume } ->
      Stream.emit (f a)
      handle resume() with go

  handle sa() with go
```

4. Inline small expressions that are used only once rather than binding them to variables:

```
-- Prefer this:
Stream.emit (f a)

-- Over this:
b = f a
Stream.emit b
```

5. Use `do` instead of `'` within function bodies to create thunks

## Effect and State Management

Handlers with state often use recursion:

```
Stream.toList : '{g, Stream a} () ->{g} [a]
Stream.toList sa =
  go acc req = match req with
    { () } -> acc
    { Stream.emit a -> resume } ->
      handle resume() with go (acc :+ a)
  handle sa() with go []
```

A common convention is to use `acc'` (with an apostrophe) to name the updated version of an accumulator variable.

## Record Types

Record types in Unison are defined as:

```
type Employee = { name : Text, age : Nat }
```

This generates accessor functions and updaters:
- `Employee.name : Employee -> Text`
- `Employee.age : Employee -> Nat`
- `Employee.name.set : Text -> Employee -> Employee`
- `Employee.age.modify : (Nat -> Nat) -> Employee -> Employee`

Example usage:

```
doubleAge : Employee -> Employee
doubleAge e = Employee.age.modify (n -> n * 2) e
```

### Important: Record Access Syntax

A common mistake is to try using dot notation for accessing record fields. In Unison, record field access is done through the generated accessor functions:

```
-- INCORRECT: ring.zero
-- CORRECT:
Ring.zero ring
```

Record types in Unison generate functions, not special field syntax.

## Namespaces and Imports

Unison uses a flat namespace with dot notation to organize code. You can import definitions using `use`:

```
use List map filter

-- Now you can use map and filter without qualifying them
evens = filter Nat.isEven [1,2,3,4]
incremented = map Nat.increment (range 0 100)
```

A wildcard import is also available:

```
use List  -- Imports all List.* definitions
```

## Collection Operations

List patterns allow for powerful matching:

```
-- Match first element of list
a +: as

-- Match last element of list
as :+ a

-- Match first two elements plus remainder
[x,y] ++ rem
```

Example implementation of `List.map`:

```
List.map : (a ->{g} b) -> [a] ->{g} [b]
List.map f as =
  go acc rem = match rem with
    [] -> acc
    a +: as -> go (acc :+ f a) as
  go [] as
```

### List Functions and Ability Polymorphism

Remember to make list functions ability-polymorphic if they take function arguments. This allows the function passed to operate with effects:

```
-- CORRECT: Ability polymorphic
List.map : (a ->{g} b) -> [a] ->{g} [b]

-- INCORRECT: Not ability polymorphic
List.map : (a -> b) -> [a] -> [b]
```

## Pattern Matching with Guards

Guards allow for conditional pattern matching:

```
List.filter : (a -> Boolean) -> [a] -> [a]
List.filter p as =
  go acc rem = match rem with
    [] -> acc
    a +: as | p a -> go (acc :+ a) as
            | otherwise -> go acc as
  go [] as
```

### Guard Style Convention

When using multiple guards with the same pattern, align subsequent guards vertically under the first one, not repeating the full pattern:

```
-- CORRECT:
a +: as | p a -> go (acc :+ a) as
        | otherwise -> go acc as

-- INCORRECT:
a +: as | p a -> go (acc :+ a) as
a +: as | otherwise -> go acc as
```

## Block Structure and Binding

In Unison, the arrow `->` introduces a block, which can contain multiple bindings followed by an expression:

```
-- A block with a helper function
dotProduct ring xs ys =
  go acc xsRem ysRem = match (xsRem, ysRem) with
    ([], _) -> acc
    (_, []) -> acc
    (x +: xs, y +: ys) ->
      nextAcc = Ring.add ring acc (Ring.mul ring x y)
      go nextAcc xs ys
  go (Ring.zero ring) xs ys
```

### Important: No `let` Keyword

Unison doesn't use a `let` keyword for bindings within blocks. Simply write the name followed by `=`:

```
-- CORRECT:
nextAcc = Ring.add ring acc (Ring.mul ring x y)

-- INCORRECT:
let nextAcc = Ring.add ring acc (Ring.mul ring x y)
```

### No `where` Clauses

Unison doesn't have `where` clauses. Helper functions must be defined in the main block before they're used.

## Error Handling

Unison uses the `Exception` ability for error handling:

```
type Failure = Failure Link.Type Text Any

-- Raising an exception
Exception.raise (Failure (typeLink Generic) "An error occurred" (Any 42))

-- Catching specific exceptions
Exception.catchOnly : Link.Type -> '{g, Exception} a ->{g, Exception} Either Failure a
```

## Text Handling

Unison calls strings `Text` and uses concatenation with `++`:

```
greeting = "Hello, " Text.++ name
```

You can use `use Text ++` to use `++` without qualification:

```
use Text ++
greeting = "Hello, " ++ name
```

### No String Interpolation

Unlike many modern languages, Unison doesn't have string interpolation. Text concatenation with `++` is the primary way to combine text values.

## The Pipeline Operator `|>`

Unison provides the `|>` operator for creating pipelines of transformations. The expression `x |> f` is equivalent to `f x`. This is particularly useful for composing multiple operations in a readable left-to-right flow:

```
use Nat *
use List filter sum map

-- Calculate the sum of odd numbers after multiplying each by 10
processNumbers : [Nat] -> Nat
processNumbers numbers =
  numbers
    |> map (x -> x * 10)   -- Multiply each number by 10
    |> filter Nat.isOdd    -- Keep only odd numbers
    |> sum                 -- Sum all remaining numbers

-- Using the function with numbers 1 through 100
result =
  range 1 101            -- Create a list from 1 to 100 (inclusive)
    |> processNumbers      -- Apply our processing function
```

This style makes the code more readable by placing the data first and showing each transformation step in sequence, similar to the pipe operator in languages like Elm, F#, or Elixir.

## Writing documentation

Documentation blocks appear just before a function or type definition. They look like so:

````
{{
``List.map f xs`` applies the function `f` to each element of `xs`.

# Examples

```
List.map Nat.increment [1,2,3]
```

```
List.map (x -> x * 100) (range 0 10)
```
}}
List.map f xs =
  go acc = cases
    [] -> acc
    hd +: tl -> go (acc :+ f hd) tl
  go [] xs
````

## Type System Without Typeclasses

Unison doesn't have typeclasses. Instead, it uses explicit dictionary passing:

```
type Ring a =
  { zero : a
  , one : a
  , add : a -> a -> a
  , mul : a -> a -> a
  , neg : a -> a
  }

dotProduct : Ring a -> [a] -> [a] -> a
dotProduct ring xs ys =
  go acc xsRem ysRem = match (xsRem, ysRem) with
    ([], _) -> acc
    (_, []) -> acc
    (x +: xs, y +: ys) ->
      nextAcc = Ring.add ring acc (Ring.mul ring x y)
      go nextAcc xs ys
  go (Ring.zero ring) xs ys
```

## Program Entry Points

Main functions in Unison can have any name:

```
main : '{IO, Exception} ()
main = do printLine "hello, world!"
```

The syntax `'{IO, Exception} ()` is sugar for `() ->{IO, Exception} ()`, representing a thunk that can use the `IO` and `Exception` abilities.

UCM (Unison Codebase Manager) is used to run or compile programs:

```
# Run directly
run main

# Compile to bytecode
compile main out

# Run compiled bytecode
ucm run.compiled out.uc
```

## Testing

Unison has built-in testing support:

```
test> Nat.tests.additionIsCommutative = test.verify do
  Each.repeat 100
  n = Random.natIn 0 1000
  m = Random.natIn 0 1000
  ensureEqual (n + m) (m + n)
```

## Lazy Evaluation

Unison is strict by default. Laziness can be achieved explicitly with `'` (outside functions) and `do` (within functions):

```
-- Outside a function, create a thunk with '
lazyComputation : '(expensive computation)
lazyComputation = '(expensiveFunction arg1 arg2)

-- Inside a function, use do
stream : '{Stream Nat} ()
stream = do
  Each.iterate ((+) 1) 0 Stream.emit
```

## Standard Library

Unison's standard library includes common data structures:
- `List` for sequences
- `Map` for key-value mappings
- `Set` for unique collections
- `Pattern` for regex matching

## Additional Resources

- Unison docs on regex patterns: https://share.unison-lang.org/@unison/base/code/main/latest/types/Pattern
- Testing documentation: https://share.unison-lang.org/@unison/base/code/main/latest/terms/test/verify
