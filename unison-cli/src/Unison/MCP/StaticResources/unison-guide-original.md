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

# Unison Remote: A Guide to Concurrent Programming

## Introduction

Unison's `Remote` ability provides a powerful abstraction for concurrent and parallel programming. It can be used either locally on a single machine or distributed across multiple nodes using Unison Cloud. This guide will walk you through the core concepts, primitives, and patterns for effective concurrent programming in Unison.

## Core Concepts

### The Remote Ability

`Remote` is a Unison ability that enables concurrent programming through a set of core operations. It provides first-class support for:

- Forking tasks to run concurrently
- Awaiting task completion
- Handling errors in distributed computations
- Working with promises for synchronization
- Using atomic references for shared state

### Locations

When using `Remote`, computations run at a specific `Location`. In a distributed setting, this might be any node in a pool managed by Unison Cloud. On a single machine, it represents the thread pool where computations run.

The `pool()` function returns a `Location` where you want your computation to execute.

## Basic Operations

### Forking and Awaiting Tasks

The foundation of concurrent programming in Unison is forking tasks and awaiting their completion:

```
t1 = Remote.forkAt pool() do 1 + 1
t2 = Remote.forkAt pool() do 2 + 2
Remote.await t1 + Remote.await t2  -- Returns 6
```

Here's what's happening:
- `Remote.forkAt` takes a location and a computation, and runs that computation at the specified location
- `Remote.await` blocks until the task completes and returns its result

Here are the API functions used:

```
Remote.await : Task a ->{Remote} a
Remote.forkAt : Location g -> '{g, Exception, Remote} t ->{Remote} Task t
```

### Error Handling

Error handling is a critical aspect of concurrent programming. Unison provides:


```
-- Try to await a task, returning Either Failure a
result = Remote.tryAwait task

-- Explicitly fail a task
Remote.fail (Failure (typeLink Generic) "Task failed" (Any 42))

match Remote.try something with
  Left err -> ...
  Right success -> ...
```

Here's a retry function that attempts a computation up to 3 times:

```
Remote.retry : '{Remote, Exception, g} a ->{Remote, Exception, g} a
Remote.retry computation =
  go attempts =
    if attempts == 0 then computation()
    else
      result = Remote.try computation
      match result with
        Right success -> success
        Left failure -> go (attempts - 1)

  go 2
```

Here are the relevant API functions for working with forking and awaiting tasks:

```
Remote.try : '{g, Exception, Remote} a ->{g, Remote} Either Failure a
Remote.tryAwait : Remote.Task a ->{Remote} Either Failure a
Remote.fail : Failure ->{Remote} x
```

## Atomic References

Unison provides atomic references for safe concurrent state manipulation:

```
-- Create a new reference
ref = Remote.Ref.new initialValue

-- Read a reference
value = Remote.Ref.read ref

-- Compare and Swap (CAS) for atomic updates
(token, currentValue) = Remote.Ref.readForCas ref
success = Remote.Ref.cas ref token currentValue newValue
```

Here's an atomic modify function using CAS:

```
Remote.Ref.atomicModify : Remote.Ref a ->{Remote} (a -> a) -> a
Remote.Ref.atomicModify ref updateFn =
  go = do
    (token, currentValue) = Remote.Ref.readForCas ref
    newValue = updateFn currentValue
    success = Remote.Ref.cas ref token currentValue newValue
    if success then newValue
    else go()

  go()
```

Here are the API functions used for working with `Remote.Ref`:

```
type Remote.Ref a
Remote.Ref.cas : Remote.Ref a -> Ref.Ticket a -> a ->{Remote} Boolean
Remote.Ref.delete : Remote.Ref a ->{Remote} ()
Remote.Ref.getThenUpdate : Remote.Ref a -> (a -> a) ->{Remote} a
Remote.Ref.modify : Remote.Ref a -> (a -> (a, b)) ->{Remote} b
Remote.Ref.new : a ->{Remote} Remote.Ref a
Remote.Ref.new.detached : a ->{Remote} Remote.Ref a
Remote.Ref.read : Remote.Ref a ->{Remote} a
Remote.Ref.readForCas : Remote.Ref a ->{Remote} (Ref.Ticket a, a)
Remote.Ref.Ref : Ref.Id -> Location.Id -> Remote.Ref a

type Remote.Ref.Ticket a
Remote.Ref.Ticket.Ticket : Nat -> Ref.Ticket a
Remote.Ref.tryCas : Remote.Ref a -> Ref.Ticket a -> a ->{Remote} Either Failure Boolean
Remote.Ref.tryDelete : Remote.Ref a ->{Remote} Either Failure ()
Remote.Ref.tryReadForCas : Remote.Ref a ->{Remote} Either Failure (Ref.Ticket a, a)
Remote.Ref.tryWrite : Remote.Ref a -> a ->{Remote} Either Failure ()
Remote.Ref.update : Remote.Ref a -> (a -> a) ->{Remote} ()
Remote.Ref.updateThenGet : Remote.Ref a -> (a -> a) ->{Remote} a
Remote.Ref.write : Remote.Ref a -> a ->{Remote} ()
```

## Promises

Promises provide synchronization between concurrent tasks:

```
-- Create an empty promise
promise = Remote.Promise.empty()

-- Write to a promise (returns true if it was empty, false if already filled)
success = Remote.Promise.write promise value

-- Blocking read from a promise
value = Remote.Promise.read promise

-- Non-blocking read (returns Optional a)
maybeValue = Remote.Promise.readNow promise
```

Promises are useful for building higher-level concurrency patterns, such as a race function:

```
Remote.race : '{Remote, Exception} a -> '{Remote, Exception} a ->{Remote, Exception} a
Remote.race computation1 computation2 =
  promise = Remote.Promise.empty()

  Remote.forkAt pool() do
    result = computation1()
    _ = Remote.Promise.write promise result
    ()

  Remote.forkAt pool() do
    result = computation2()
    _ = Remote.Promise.write promise result
    ()

  Remote.Promise.read promise
```

Here are the API functions for working with promises:

```
type Remote.Promise a

Remote.Promise.delete : Remote.Promise a ->{Remote} ()
Remote.Promise.empty : '{Remote} Remote.Promise a
Remote.Promise.empty.detached! : {Remote} (Remote.Promise a)

Remote.Promise.read : Remote.Promise a ->{Remote} a
Remote.Promise.readNow : Remote.Promise a ->{Remote} Optional a
Remote.Promise.tryDelete : Remote.Promise a ->{Remote} Either Failure ()
Remote.Promise.tryRead : Remote.Promise a ->{Remote} Either Failure a
Remote.Promise.tryReadNow : Remote.Promise a ->{Remote} Either Failure (Optional a)
Remote.Promise.tryWrite : Remote.Promise a -> a ->{Remote} Either Failure Boolean
Remote.Promise.write : Remote.Promise a -> a ->{Remote} Boolean
Remote.Promise.write_ : Remote.Promise a -> a ->{Remote} ()
```

## Structured Concurrency

Unison's `Remote` ability supports structured concurrency, ensuring that child tasks don't outlive their parent context:

- By default, a forked task, promise, or ref will be cleaned up when the parent task completes
- For detached resources that persist beyond their parent's lifetime, use:
  - `Remote.Ref.new.detached` (for refs)
  - `Promise.empty.detached!` (for promises)
  - `Remote.detach pool() r` (for tasks)

When using detached resources, you're responsible for cleanup:
- `Remote.Ref.delete` (for refs)
- `Remote.Promise.delete` (for promises)
- `Remote.cancel` (for tasks)

## Finalizers

You can use `Remote.addFinalizer : (Outcome ->{Remote} ()) ->{Remote} ()` to add logic that should be run when a block completes, either due to success, failure, or cancellation. Here's an example:

```
addFinalizer do someCleanupFunction xyz
```

If you need to do something different you can pattern match on the `Outcome`:

```
type Remote.Outcome = Completed | Cancelled | Failed Failure
```

```
addFinalizer cases
  Completed -> -- the success case
  Cancelled -> -- if the parent task was cancelled
  Failed err -> -- if the task failed with some error
```

### Concurrent Queue

Here's an implementation of a concurrent queue using `Remote` primitives:

```
type Queue a = Queue (Remote.Ref ([a], Optional (Remote.Promise ())))

Queue.underlying = cases Queue r -> r

Queue.new : '{Remote} Queue a
Queue.new =
  ref = Remote.Ref.new ([], None)
  Queue ref

Queue.enqueue : Queue a -> a ->{Remote} ()
Queue.enqueue q item =
  (token, (items, waiter)) = Remote.Ref.readForCas (Queue.underlying ref)

  match waiter with
    None ->
      success = Remote.Ref.cas ref token (items, None) (items :+ item, None)
      if success then () else Queue.enqueue (Queue ref) item

    Some promise ->
      success = Remote.Ref.cas ref token (items, Some promise) (items :+ item, None)
      if success then
        _ = Remote.Promise.write promise ()
        ()
      else Queue.enqueue (Queue ref) item

Queue.dequeue : Queue a ->{Remote} a
Queue.dequeue q =
  (token, (items, _)) = Remote.Ref.readForCas (Queue.underlying q)

  match items with
    [] ->
      promise = Remote.Promise.empty()
      success = Remote.Ref.cas ref token ([], None) ([], Some promise)
      if success then
        Remote.Promise.read promise
        Queue.dequeue (Queue ref)
      else Queue.dequeue (Queue ref)

    item +: rest ->
      success = Remote.Ref.cas ref token (items, _) (rest, None)
      if success then item else Queue.dequeue (Queue ref)

Queue.size : Queue a ->{Remote} Nat
Queue.size q =
  (items, _) = Remote.Ref.read (Queue.underlying ref)
  List.size items
```

### Bounded Parallel Map with Retry

This function processes a list in parallel with bounded concurrency and retries failed tasks:

```
Remote.boundedParMapWithRetry : Nat -> [b] -> (b ->{Remote, Exception} a) ->{Remote, Exception} [a]
Remote.boundedParMapWithRetry maxConcurrent inputs fn =
  processChunk : [b] ->{Remote, Exception} [a]
  processChunk chunk =
    processWithRetry : b ->{Remote, Exception} a
    processWithRetry input =
      retry : Nat ->{Remote, Exception} a
      retry attemptsLeft =
        if attemptsLeft == 0 then fn input
        else
          task = Remote.forkAt pool() do fn input
          result = Remote.tryAwait task

          match result with
            Right success -> success
            Left failure -> retry (attemptsLeft - 1)

      retry 2

    go : [b] -> [a] ->{Remote, Exception} [a]
    go remaining acc =
      match remaining with
        [] -> acc
        input +: rest ->
          result = processWithRetry input
          go rest (acc :+ result)

    go chunk []

  chunks = List.chunk maxConcurrent inputs

  tasks = List.map (chunk ->
    Remote.forkAt pool() do processChunk chunk
  ) chunks

  results = List.map Remote.await tasks
  List.flatten results
```

## Distributed Computing Considerations

When using `Remote` with Unison Cloud, additional considerations come into play:

- Tasks may fail due to node failures or network issues, even if the computation itself is correct
- You can control task placement using location functions:
  - `pool()` - Pick a random available node
  - `Remote.near pool() (workLocation t)` - Fork at the same location as task `t`
  - `Remote.far pool() loc` - Pick a location different from `loc`

## Timing and Timeouts

The `Remote` ability provides functions for introducing delays and implementing timeouts in concurrent operations:

### Sleep

You can pause execution of a task using:

```
Remote.sleepMicroseconds : Nat ->{Remote} ()
```

This is useful for implementing retry with backoff, polling intervals, or simply delaying operations:

```
-- Sleep for 1 second (1,000,000 microseconds)
Remote.sleepMicroseconds 1000000
```

### Timeouts

Timeouts are essential for preventing operations from blocking indefinitely. Unison provides:

```
Remote.timeout : Nat -> '{Remote, g} a ->{Remote, g} Optional a
```

This function runs a computation and returns `None` if the computation doesn't complete within the specified microseconds:

```
-- Try to run a computation with a 5-second timeout
result = Remote.timeout 5000000 do
  expensiveOperation()

match result with
  Some value -> use the value
  None -> handle the timeout case
```

You can combine timeout with race patterns for more complex scenarios:

```
withTimeout : Nat -> '{Remote, g} a ->{Remote, g} Either Text a
withTimeout microseconds computation =
  Remote.race
    (do
      Remote.sleepMicroseconds microseconds
      Remote.pure (Left "Operation timed out"))
    (do
      result = computation()
      Remote.pure (Right result))
```

## Performance Considerations

- When forking tasks across network boundaries, ensure the computation is substantial enough to justify the network overhead
- Even with local green threading, task granularity matters
- Use bounded parallelism functions like `Remote.boundedParMap` to limit concurrent tasks

## Conclusion

The `Remote` ability in Unison provides a powerful abstraction for concurrent and distributed programming. By understanding its core primitives—tasks, promises, and refs—along with structured concurrency principles, you can build robust concurrent applications that scale from a single machine to distributed environments.

This guide covered the fundamentals, but the true power of `Remote` emerges when you combine these primitives to build higher-level abstractions tailored to your specific use cases.



# Base Library Functions

Here is a list of all definitions in Unison's base library, including type signatures for functions. Use these where possible when writing Unison code.

```
structural ability abilities.Abort
abilities.Abort.abort : {Abort} a
abilities.Abort.abortWhen : Boolean ->{Abort} ()
abilities.Abort.toBug : '{g, Abort} a ->{g} a
abilities.Abort.toDefault : '{g} a -> '{g, Abort} a -> '{g} a
abilities.Abort.toDefault! : '{g} a -> '{g, Abort} a ->{g} a
abilities.Abort.toDefault!.handler : '{g} a -> Request {Abort} a ->{g} a
abilities.Abort.toDefaultValue : a -> '{g, Abort} a -> '{g} a
abilities.Abort.toDefaultValue! : a -> '{g, Abort} a ->{g} a
abilities.Abort.toDefaultValue!.handler : a -> Request {Abort} a -> a
abilities.Abort.toException : '{g, Abort} a -> Type -> Text ->{g, Exception} a
abilities.Abort.toGenericException : Text -> e -> '{g, Abort} a ->{g, Exception} a
abilities.Abort.toLazyThrow : 'e -> '{g, Abort} a ->{g, Throw e} a
abilities.Abort.toOptional : '{g, Abort} a -> '{g} Optional a
abilities.Abort.toOptional! : '{g, Abort} a ->{g} Optional a
abilities.Abort.toThrow : e -> '{g, Abort} a ->{g, Throw e} a
structural ability abilities.Ask a
abilities.Ask.ask : {Ask a} a
abilities.Ask.map : (a ->{g1} b) -> '{g2, Ask b} r -> '{g1, g2, Ask a} r
abilities.Ask.map! : (a ->{g1} b) -> '{g2, Ask b} r ->{g1, g2, Ask a} r
abilities.Ask.map!.example : Text
abilities.Ask.provide : a -> '{g, Ask a} r ->{g} r
abilities.Ask.provide.handler : a -> Request {Ask a} r -> r
abilities.Ask.provide' : '{g} a -> '{g, Ask a} r ->{g} r
abilities.Ask.provide'.handler : '{g} a -> Request {Ask a} r ->{g} r
abilities.Ask.toStore : '{g, Ask a} r ->{g, Store a} r
abilities.Ask.toStore.handler : Request {Ask a} r ->{Store a} r
ability abilities.Clock
abilities.Clock.elapsed : {Clock} Duration
abilities.Clock.now : {Clock} Instant
abilities.Clock.runClock : (a ->{g, Clock} b) -> a ->{g, IO, Exception} b
structural ability abilities.Each
abilities.Each.allowThrow : '{Throw e, Stream a} () ->{Each, Throw e} a
abilities.Each.append : '{g, Each} a -> '{g, Each} a ->{g, Each} a
abilities.Each.count : '{g, Each} a ->{g} Nat
abilities.Each.each : [a] ->{Each} a
abilities.Each.eachCapture : Pattern t -> t ->{Each} t
abilities.Each.eachChar : Text ->{Each} Char
abilities.Each.fail : '{Each} a
abilities.Each.filter : (a ->{e} Boolean) -> a ->{e, Each} a
abilities.Each.first : '{g, Each} a ->{g, Abort} a
abilities.Each.fromAbort : '{g, Abort} a ->{g, Each} a
abilities.Each.fromException : '{g, Exception} a ->{g, Each} a
abilities.Each.fromThrow : '{g, Throw e} a ->{g, Each} a
abilities.Each.guard : Boolean ->{Each} ()
abilities.Each.ifThenElse : '{g, Each} a -> (a ->{g, Each} b) -> '{g, Each} b ->{g, Each} b
abilities.Each.ifThenElse.examples.jimsAncestors : [Text]
abilities.Each.ifThenElse.examples.notJimsAncestors : [Text]
abilities.Each.ifThenElse.examples.notJimsAncestors' : [Text]
abilities.Each.interleave : '{g, Each} a -> '{g, Each} a ->{g, Each} a
abilities.Each.interleaveMap : (a ->{g, Each} b) -> '{g, Each} a ->{g, Each} b
abilities.Each.lazily : '{Stream a} () ->{Each} a
abilities.Each.left : Either a b ->{Each} a
abilities.Each.limit : Nat -> '{g, Each} a ->{g, Each} a
abilities.Each.negate : '{g, Each} a ->{g, Each} ()
abilities.Each.observe : '{g, Each} a ->{g} Optional a
abilities.Each.once : '{g, Each} a ->{g, Each} a
abilities.Each.optionally : Optional a ->{Each} a
abilities.Each.range : Nat -> Nat ->{Each} Nat
abilities.Each.rangeClosed : Nat -> Nat ->{Each} Nat
abilities.Each.repeat : Nat ->{Each} ()
abilities.Each.repeatForever : '{Each} ()
abilities.Each.right : Either a b ->{Each} b
abilities.Each.run : '{g, Each} a ->{g} ()
abilities.Each.split : '{g, Each} a ->{g} Optional (a, '{g, Each} a)
abilities.Each.toArray : '{g, Each} a ->{g, Exception} data.Array a
abilities.Each.toList : '{g, Each} a ->{g} [a]
abilities.Each.toOptional : '{g, Each} a ->{g} Optional a
abilities.Each.toStream : '{g, Each} a -> '{g, Stream a} ()
abilities.Each.toStream! : '{g, Each} a ->{g, Stream a} ()
structural ability abilities.Exception
abilities.Exception.abortOnException : '{g, Exception} a ->{g, Abort} a
abilities.Exception.bracket : '{g, Exception} r
                              -> (r ->{g, Exception} ())
                              -> (r ->{g, Exception} a)
                              ->{g, Exception} a
abilities.Exception.catch : '{g, Exception} a ->{g} Either Failure a
abilities.Exception.catchMany : [Type]
                                -> '{g, Exception} a
                                ->{g, Exception} Either Failure a
abilities.Exception.catchOnly : Type -> '{g, Exception} a ->{g, Exception} Either Failure a
abilities.Exception.failOnError : Text -> Either e a ->{Exception} a
abilities.Exception.finally : '{g, Exception} () -> '{g, Exception} a ->{g, Exception} a
type abilities.Exception.Generic
abilities.Exception.Generic.failure : Text -> a -> Failure
abilities.Exception.hush : '{g, Exception} t ->{g} Optional t
abilities.Exception.onException : (Failure ->{e} ())
                                  -> '{g, Exception} a
                                  ->{e, g, Exception} a
abilities.Exception.orElse : '{f} a -> '{g, Exception} a ->{f, g} a
abilities.Exception.raise : Failure ->{Exception} x
abilities.Exception.raiseFailure : Type -> Text -> a ->{Exception} x
abilities.Exception.raiseGeneric : Text -> a ->{Exception} b
abilities.Exception.reraise : Either Failure a ->{Exception} a
abilities.Exception.unsafeRun! : '{g, Exception} a ->{g} a
abilities.force : '{e} a ->{e} a
abilities.forever : '{e} a ->{e} b
abilities.forever' : '{g} a -> '{g} b
ability abilities.Label
abilities.Label.formatLabels : [(Text, Map Text Text)] -> Text
abilities.Label.getLabels : '{g, Label} a ->{g} [(Text, Map Text Text)]
abilities.Label.label : Text -> a ->{Label} ()
abilities.Label.labeled : Text -> '{g} a ->{g, Label} a
abilities.Label.popScope : {Label} ()
abilities.Label.pushScope : Text ->{Label} ()
abilities.Label.run : '{g, Label} r ->{g} ([(Text, Map Text Text)], r)
abilities.Label.runToText : '{g, Label} a ->{g} (Text, a)
abilities.Label.toText : '{g, Label} a ->{g} Text
ability abilities.Random
abilities.Random.boolean : '{Random} Boolean
abilities.Random.bytes : Nat ->{Random} Bytes
abilities.Random.bytes.base32Hex : '{Random} Bytes
abilities.Random.char.ascii.control : '{Random} Char
abilities.Random.char.ascii.lower : '{Random} Char
abilities.Random.char.ascii.printable : '{Random} Char
abilities.Random.char.ascii.upper : '{Random} Char
abilities.Random.char.base32Hex : '{Random} Char
abilities.Random.char.digit : '{Random} Char
abilities.Random.char.hex : '{Random} Char
abilities.Random.char.interval : Char -> Char ->{Random} Char
abilities.Random.char.unicode : '{Random} Char
abilities.Random.either : '{Random} r -> '{Random} r ->{Random} r
abilities.Random.exponential : Float ->{Random} Float
abilities.Random.float : '{Random} Float
abilities.Random.functionOf : '{g, Random} b ->{Random} (∀ a. a ->{g} b)
abilities.Random.impl.bytesFromNats : '{g, Random} Nat -> Nat ->{g, Random} Bytes
abilities.Random.int : '{Random} Int
abilities.Random.intIn : Int -> Int ->{Random} Int
abilities.Random.lcg : Nat -> '{g, Random} t ->{g} t
abilities.Random.lcg.handler : Nat -> Request {Random} a -> a
abilities.Random.listOf : '{g, Random} a -> '{g, Random} Nat ->{g, Random} [a]
abilities.Random.mapOf : '{g1, Random} k
                         -> '{g2, Random} v
                         -> '{g1, g2, Random} Nat
                         ->{g1, g2, Random} Map k v
abilities.Random.nat : '{Random} Nat
abilities.Random.nat.natsWithSum : Nat -> Nat ->{Random} [Nat]
abilities.Random.nat! : {Random} Nat
abilities.Random.natIn : Nat -> Nat ->{Random} Nat
abilities.Random.oneOf : [a] ->{Random} a
abilities.Random.oneOfNonempty : List.Nonempty a ->{Random} a
abilities.Random.optional : '{g, Random} a ->{g} '{g, Random} Optional a
abilities.Random.optional! : '{g, Random} a ->{g, Random} Optional a
structural type abilities.Random.RNG
abilities.Random.RNG.fromLcg : Nat -> RNG
abilities.Random.RNG.fromSplitmix : Nat -> RNG
abilities.Random.RNG.RNG : (∀ g a. '{g, Random} a ->{g} a) -> RNG
abilities.Random.RNG.run : RNG -> '{g, Random} a ->{g} a
abilities.Random.RNG.split : RNG -> (RNG, RNG)
abilities.Random.run : '{g, Random} a ->{g, IO} a
abilities.Random.shuffle : [a] ->{Random} [a]
abilities.Random.split : '{Random} ('{g, Random} t ->{g} t)
abilities.Random.split! : {Random} (∀ g a. '{g, Random} a ->{g} a)
abilities.Random.splitmix : Nat -> '{g, Random} t ->{g} t
abilities.Random.splitmix.handler : Nat -> Nat -> Request {Random} a2 -> a2
abilities.Random.splitmix.impl.goldenGamma : Nat
abilities.Random.splitmix.impl.mix64 : Nat -> Nat
abilities.Random.splitmix.impl.mix64variant13 : Nat -> Nat
abilities.Random.splitmix.impl.mixGamma : Nat -> Nat
abilities.Random.splitmix.impl.shiftXor : Nat -> Nat -> Nat
abilities.Random.splitmix.impl.shiftXorMultiply : Nat -> Nat -> Nat -> Nat
abilities.Random.splits : (a ->{g1} Nat)
                          -> (Nat -> a ->{g2} (a, a))
                          -> Nat
                          -> a
                          -> '{g1, g2, Random, Stream a} ()
abilities.Random.splits.bytes : Nat -> Bytes -> '{Random, Stream Bytes} ()
abilities.Random.splits.list : Nat -> [a] -> '{Random, Stream [a]} ()
abilities.Random.splits.text : Nat -> Text -> '{Random, Stream Text} ()
abilities.Random.text.base32Hex : '{Random} Text
abilities.Random.text.ofChars : '{Random} Char -> Nat ->{Random} Text
abilities.Random.weighted : [(Nat, '{g, Random} a)] ->{g} '{g, Random} a
abilities.Random.weighted.fromStream : '{g, Stream (Nat, '{g, Random} a)} r
                                       ->{g} '{g, Random} a
abilities.repeat : Nat -> '{e} () ->{e} ()
builtin type abilities.Request
structural ability abilities.Store a
abilities.Store.accumulateLeft : (a ->{g, Store s} b) -> s -> [a] ->{g} [b]
abilities.Store.accumulateRight : (a ->{g, Store s} b) -> s -> [a] ->{g} [b]
abilities.Store.examples.stack.add : '{Abort, Store [Int]} ()
abilities.Store.examples.stack.binop : (a ->{e} a ->{g} a) ->{e, g, Abort, Store [a]} ()
abilities.Store.examples.stack.pop : '{Abort, Store [a]} a
abilities.Store.examples.stack.push : a ->{Store [a]} ()
abilities.Store.examples.stack.runStack : '{Abort, Store [Int]} a -> Optional a
abilities.Store.get : {Store a} a
abilities.Store.get.examples.ex1 : Nat
abilities.Store.getOrUpdate : k -> '{Store (Map k v)} v ->{Store (Map k v)} v
abilities.Store.getOrUpdate.examples.fibonacci : Nat ->{Store (Map Nat Natural)} Natural
abilities.Store.local : (a ->{g} b)
                        -> (a ->{g} b ->{g} a)
                        -> '{g, Store b} v
                        -> '{g, Store a} v
abilities.Store.local.deprecated : a -> '{g, Store a} v ->{g, Store a} v
abilities.Store.local.deprecated.examples.ex1 : (Nat, Nat, Nat)
abilities.Store.local! : (a ->{g} b)
                         -> (a ->{g} b ->{g} a)
                         -> '{g, Store b} v
                         ->{g, Store a} v
abilities.Store.local!.examples.ex1 : (Text, Nat)
abilities.Store.modify : (a ->{g} a) ->{g, Store a} ()
abilities.Store.modify.examples.increment : Nat
abilities.Store.put : a ->{Store a} ()
abilities.Store.put.examples.ex1 : Nat
abilities.Store.unfold : s -> '{g, Abort, Store s} r -> '{g} [r]
abilities.Store.unfold! : s -> '{g, Abort, Store s} r ->{g} [r]
abilities.Store.withInitialValue : a -> '{g, Store a} v ->{g} v
abilities.Store.withInitialValue.handler : a -> Request (Store a) v -> v
structural ability abilities.Throw e
abilities.Throw.catchWith : (e ->{g1} a) -> '{g2, Throw e} a ->{g1, g2} a
abilities.Throw.throw : e ->{Throw e} a
abilities.Throw.toBug : '{g, Throw e} o ->{g} o
abilities.Throw.toEither : '{g, Throw e} a ->{g} Either e a
abilities.Throw.toEither.handler : Request {Throw e} a -> Either e a
abilities.Throw.toException : (e ->{g1} Failure)
                              -> '{g2, Throw e} a
                              ->{g1, g2, Exception} a
abilities.Throw.toOptional : '{g, Throw e} a ->{g} Optional a
abilities.Throw.unwrap : '{g, Throw a} a -> '{g} a
abilities.Throw.unwrap! : '{g, Throw a} a ->{g} a
ability abilities.Wait
abilities.Wait.runWait : (a ->{g, Wait} b) -> a ->{g, IO, Exception} b
abilities.Wait.wait : Duration ->{Wait} ()
builtin type Any
Any.Any : a -> Any
Any.unsafeExtract : Any -> a
builtin type Boolean
Boolean.!= : Boolean -> Boolean -> Boolean
Boolean.< : Boolean -> Boolean -> Boolean
Boolean.<= : Boolean -> Boolean -> Boolean
Boolean.== : Boolean -> Boolean -> Boolean
Boolean.> : Boolean -> Boolean -> Boolean
Boolean.>= : Boolean -> Boolean -> Boolean
Boolean.and : Boolean -> Boolean -> Boolean
                                -> (Boolean ->{g} Boolean ->{g1} Boolean)
Boolean.eq : Boolean -> Boolean -> Boolean
Boolean.given : Boolean -> Boolean -> Boolean
Boolean.gt : Boolean -> Boolean -> Boolean
Boolean.gteq : Boolean -> Boolean -> Boolean
Boolean.iff : Boolean -> Boolean -> Boolean
Boolean.implies : Boolean -> Boolean -> Boolean
Boolean.lt : Boolean -> Boolean -> Boolean
Boolean.lteq : Boolean -> Boolean -> Boolean
Boolean.nand : Boolean -> Boolean -> Boolean
Boolean.neq : Boolean -> Boolean -> Boolean
Boolean.nor : Boolean -> Boolean -> Boolean
Boolean.not : Boolean -> Boolean
Boolean.or : Boolean -> Boolean -> Boolean
Boolean.toNat : Boolean -> Nat
Boolean.toText : Boolean -> Text
Boolean.until : (a ->{e} Boolean) -> '{e} a ->{e} a
Boolean.when : Boolean -> '{e} () ->{e} ()
Boolean.while : (a ->{e} Boolean) -> '{e} a ->{e} a
Boolean.xor : Boolean -> Boolean -> Boolean
bug : a -> b
bug.impossible : Request Exception a -> a
builtin type Bytes
Bytes.++ : Bytes -> Bytes -> Bytes
Bytes.== : Bytes -> Bytes -> Boolean
Bytes.at : Nat -> Bytes -> Optional Nat
Bytes.at! : Nat -> Bytes ->{Abort} Nat
Bytes.base32Hex : Bytes -> Text
Bytes.base32Hex.b32h : Nat -> Char
Bytes.base32Hex.encodeChunk : Nat -> Nat -> Text
Bytes.base32Hex.grab : Bytes -> Optional Hex32Piece
Bytes.base32Hex.grab1 : Bytes -> Optional Hex32Piece
structural type Bytes.base32Hex.Hex32Piece
Bytes.base32Hex.Hex32Piece.Double : Nat -> Nat -> Nat -> Bytes -> Hex32Piece
Bytes.base32Hex.Hex32Piece.Single : Nat -> Nat -> Bytes -> Hex32Piece
Bytes.constantTimeEqual : Bytes -> Bytes -> Boolean
Bytes.decodeNat16be : Bytes -> Optional (Nat, Bytes)
Bytes.decodeNat16le : Bytes -> Optional (Nat, Bytes)
Bytes.decodeNat16sbe : Nat -> Bytes -> ([Nat], Bytes)
Bytes.decodeNat32be : Bytes -> Optional (Nat, Bytes)
Bytes.decodeNat32le : Bytes -> Optional (Nat, Bytes)
Bytes.decodeNat32sbe : Nat -> Bytes -> ([Nat], Bytes)
Bytes.decodeNat64be : Bytes -> Optional (Nat, Bytes)
Bytes.decodeNat64le : Bytes -> Optional (Nat, Bytes)
Bytes.decodeNat64sbe : Nat -> Bytes -> ([Nat], Bytes)
Bytes.drop : Nat -> Bytes -> Bytes
Bytes.empty : Bytes
Bytes.encodeNat16be : Nat -> Bytes
Bytes.encodeNat16le : Nat -> Bytes
Bytes.encodeNat32be : Nat -> Bytes
Bytes.encodeNat32le : Nat -> Bytes
Bytes.encodeNat64be : Nat -> Bytes
Bytes.encodeNat64le : Nat -> Bytes
Bytes.flatten : Bytes -> Bytes
Bytes.fromBase16 : Bytes ->{Exception} Bytes
Bytes.fromBase16.impl : Bytes -> Either Text Bytes
Bytes.fromBase32 : Bytes ->{Exception} Bytes
Bytes.fromBase32.impl : Bytes -> Either Text Bytes
Bytes.fromBase32Hex : Bytes -> Either Text Bytes
Bytes.fromBase32Hex.alignment : Nat -> Nat
Bytes.fromBase32Hex.finalize : Nat -> Nat -> Bytes
Bytes.fromBase32Hex.h32b : Nat -> Either Text Nat
Bytes.fromBase32Hex.push : Nat -> Nat -> Nat -> Either Nat (Bytes, Nat)
Bytes.fromBase64 : Bytes ->{Exception} Bytes
Bytes.fromBase64.impl : Bytes -> Either Text Bytes
Bytes.fromBase64UrlUnpadded : Bytes -> Either Text Bytes
Bytes.fromHex : Text ->{Exception} Bytes
Bytes.fromHex.impl : Text -> Either Text Bytes
Bytes.fromList : [Nat] ->{Exception} Bytes
Bytes.fromList.impl : [Nat] -> Bytes
Bytes.gzip.compress : Bytes -> Bytes
Bytes.gzip.decompress : Bytes ->{Exception} Bytes
Bytes.gzip.decompress.impl : Bytes -> Either Text Bytes
Bytes.indexOf : Bytes -> Bytes -> Optional Nat
Bytes.isEmpty : Bytes -> Boolean
Bytes.isPrefixOf : Bytes -> Bytes -> Boolean
Bytes.shiftLeft : Nat -> Bytes -> Bytes
Bytes.shiftRight : Nat -> Bytes -> Bytes
Bytes.size : Bytes -> Nat
Bytes.splitAt : Nat -> Bytes -> (Bytes, Bytes)
Bytes.take : Nat -> Bytes -> Bytes
Bytes.toBase16 : Bytes -> Bytes
Bytes.toBase16.text : Bytes -> Text
Bytes.toBase16.text.deprecated : Bytes -> Text
Bytes.toBase32 : Bytes -> Bytes
Bytes.toBase32Hex : Bytes -> Bytes
Bytes.toBase32Hex.text : Bytes -> Text
Bytes.toBase64 : Bytes -> Bytes
Bytes.toBase64UrlUnpadded : Bytes -> Bytes
Bytes.toHex : Bytes -> Text
Bytes.toHex.deprecated : Bytes -> Text
Bytes.toList : Bytes -> [Nat]
Bytes.toNat64sbe : Bytes -> ([Nat], Bytes)
Bytes.truncateLeft : Nat -> Bytes -> Bytes
Bytes.truncateRight : Nat -> Bytes -> Bytes
Bytes.zlib.compress : Bytes -> Bytes
Bytes.zlib.decompress : Bytes ->{Exception} Bytes
Bytes.zlib.decompress.impl : Bytes -> Either Text Bytes
builtin type Char
Char.!= : Char -> Char -> Boolean
Char.< : Char -> Char -> Boolean
Char.<= : Char -> Char -> Boolean
Char.== : Char -> Char -> Boolean
Char.> : Char -> Char -> Boolean
Char.>= : Char -> Char -> Boolean
Char.ascii.fromBase36Digit : Nat -> Optional Char
Char.ascii.isAlphaNum : Char -> Boolean
Char.ascii.isAscii : Char -> Boolean
Char.ascii.isBlank : Char -> Boolean
Char.ascii.isControl : Char -> Boolean
Char.ascii.isDigit : Char -> Boolean
Char.ascii.isGraph : Char -> Boolean
Char.ascii.isHexDigit : Char -> Boolean
Char.ascii.isLetter : Char -> Boolean
Char.ascii.isLower : Char -> Boolean
Char.ascii.isPrint : Char -> Boolean
Char.ascii.isPunct : Char -> Boolean
Char.ascii.isSpace : Char -> Boolean
Char.ascii.isUpper : Char -> Boolean
Char.ascii.lowerUpperDiff : Int
Char.ascii.toBase36Digit : Char -> Optional Nat
Char.ascii.toLower : Char -> Char
Char.ascii.toUpper : Char -> Char
builtin type Char.Class
Char.Class.+ : Class -> Class -> Class
Char.Class.- : Class -> Class -> Class
Char.Class.alphanumeric : Class
Char.Class.and : Class -> Class -> Class
Char.Class.any : Class
Char.Class.anyOf : [Char] -> Class
Char.Class.ascii : Class
Char.Class.control : Class
Char.Class.digit : Class
Char.Class.fromChar : Char -> Class
Char.Class.hexDigit : Class
Char.Class.in : Text -> Class
Char.Class.is : Class -> Char -> Boolean
Char.Class.letter : Class
Char.Class.lower : Class
Char.Class.mark : Class
Char.Class.not : Class -> Class
Char.Class.number : Class
Char.Class.or : Class -> Class -> Class
Char.Class.printable : Class
Char.Class.punctuation : Class
Char.Class.range : Char -> Char -> Class
Char.Class.separator : Class
Char.Class.symbol : Class
Char.Class.upper : Class
Char.Class.visible : Class
Char.Class.whitespace : Class
Char.Class.word : Class
Char.eq : Char -> Char -> Boolean
Char.fromNat : Nat -> Optional Char
Char.fromNat.impl : Nat -> Char
Char.gt : Char -> Char -> Boolean
Char.gteq : Char -> Char -> Boolean
Char.inRange : Char -> Char -> Char -> Boolean
Char.isWhitespace : Char -> Boolean
Char.lt : Char -> Char -> Boolean
Char.lteq : Char -> Char -> Boolean
Char.neq : Char -> Char -> Boolean
Char.range : Char -> Char -> [Char]
Char.rangeClosed : Char -> Char -> [Char]
Char.toLowercase : Char -> Char
Char.toNat : Char -> Nat
Char.toText : Char -> Text
Char.toUppercase : Char -> Char
Char.toUtf8 : Char -> Bytes
type crypto.CryptoFailure
type crypto.Ed25519.PrivateKey
crypto.Ed25519.PrivateKey.PrivateKey : Bytes -> Ed25519.PrivateKey
type crypto.Ed25519.PublicKey
crypto.Ed25519.PublicKey.PublicKey : Bytes -> Ed25519.PublicKey
crypto.Ed25519.PublicKey.toBytes : Ed25519.PublicKey -> Bytes
crypto.Ed25519.sign : Ed25519.PrivateKey
                      -> Ed25519.PublicKey
                      -> Bytes
                      ->{Exception} Ed25519.Signature
crypto.Ed25519.sign.impl : Bytes -> Bytes -> Bytes -> Either Failure Bytes
type crypto.Ed25519.Signature
crypto.Ed25519.Signature.Signature : Bytes -> Ed25519.Signature
crypto.Ed25519.verify : Ed25519.PublicKey
                        -> Bytes
                        -> Ed25519.Signature
                        ->{Exception} Boolean
crypto.Ed25519.verify.impl : Bytes -> Bytes -> Bytes -> Either Failure Boolean
crypto.hash : HashAlgorithm -> a -> Bytes
builtin type crypto.HashAlgorithm
crypto.HashAlgorithm.Blake2b_256 : HashAlgorithm
crypto.HashAlgorithm.Blake2b_512 : HashAlgorithm
crypto.HashAlgorithm.Blake2s_256 : HashAlgorithm
crypto.HashAlgorithm.Md5 : HashAlgorithm
crypto.HashAlgorithm.Sha1 : HashAlgorithm
crypto.HashAlgorithm.Sha2_256 : HashAlgorithm
crypto.HashAlgorithm.Sha2_512 : HashAlgorithm
crypto.HashAlgorithm.Sha3_256 : HashAlgorithm
crypto.HashAlgorithm.Sha3_512 : HashAlgorithm
crypto.hashBytes : HashAlgorithm -> Bytes -> Bytes
crypto.hmac : HashAlgorithm -> Bytes -> a -> Bytes
crypto.hmac.verify : HashAlgorithm -> Bytes -> a -> Bytes -> Boolean
crypto.hmac.verifyBytes : HashAlgorithm -> Bytes -> Bytes -> Bytes -> Boolean
crypto.hmacBytes : HashAlgorithm -> Bytes -> Bytes -> Bytes
type crypto.Rsa.PrivateKey
crypto.Rsa.PrivateKey.PrivateKey : Bytes -> Rsa.PrivateKey
type crypto.Rsa.PublicKey
crypto.Rsa.PublicKey.PublicKey : Bytes -> Rsa.PublicKey
crypto.Rsa.sign : Rsa.PrivateKey -> Bytes ->{Exception} Rsa.Signature
crypto.Rsa.sign.impl : Bytes -> Bytes -> Either Failure Bytes
type crypto.Rsa.Signature
crypto.Rsa.Signature.Signature : Bytes -> Rsa.Signature
crypto.Rsa.verify : Rsa.PublicKey -> Bytes -> Rsa.Signature ->{Exception} Boolean
crypto.Rsa.verify.impl : Bytes -> Bytes -> Bytes -> Either Failure Boolean
structural type data.Array a
data.Array.append : data.Array a -> data.Array a -> data.Array a
data.Array.Arr : Nat -> Nat -> data.Array.Raw a -> data.Array a
type data.Array.ArrayFailure
data.Array.ArrayFailure.raise : Text -> a ->{Exception} b
data.Array.at : Nat -> data.Array a -> Optional a
data.Array.at! : Nat -> data.Array a ->{Abort} a
data.Array.cons : a -> data.Array a -> data.Array a
data.Array.copy : Nat
                  -> Nat
                  -> Nat
                  -> data.Array a
                  -> mutable.Array g a
                  ->{g, Exception} ()
data.Array.drop : Nat -> data.Array a ->{Exception} data.Array a
data.Array.empty : data.Array a
data.Array.fill : Nat -> a -> data.Array a
data.Array.find : (a ->{g} Boolean) -> data.Array a ->{g} Optional a
data.Array.findLast : (a ->{g} Boolean) -> data.Array a ->{g} Optional a
data.Array.firstIndexOf : (a ->{g} Boolean) -> data.Array a ->{g} Optional Nat
data.Array.foldLeft : (a ->{g} b ->{h} a) -> a -> data.Array b ->{g, h} a
data.Array.foldRight : (a ->{g} b ->{h} b) -> b -> data.Array a ->{g, h} b
data.Array.fromList : [a] -> data.Array a
data.Array.isEmpty : data.Array a -> Boolean
data.Array.lastIndexOf : (a ->{g} Boolean) -> data.Array a ->{g} Optional Nat
data.Array.map : (a ->{g} b) -> data.Array a ->{g} data.Array b
data.Array.new! : a
                  -> Nat
                  -> (∀ s. mutable.Array {Scope s} a ->{g, Exception, Scope s} ())
                  ->{g} data.Array a
data.Array.of : x -> Nat -> data.Array x
data.Array.randomChoice : data.Array a ->{Exception, Random} a
builtin type data.Array.Raw
data.Array.Raw.copyTo! : mutable.Array.Raw g a
                         -> Nat
                         -> data.Array.Raw a
                         -> Nat
                         -> Nat
                         ->{g, Exception} ()
data.Array.Raw.fromList : [a] -> data.Array.Raw a
data.Array.Raw.read : data.Array.Raw a -> Nat ->{Exception} a
data.Array.Raw.size : data.Array.Raw a -> Nat
data.Array.read : data.Array a -> Nat ->{Exception} a
data.Array.singleton : a -> data.Array a
data.Array.size : data.Array a -> Nat
data.Array.slice : Nat -> Nat -> data.Array a ->{Exception} data.Array a
data.Array.snoc : data.Array a -> a -> data.Array a
data.Array.take : Nat -> data.Array t ->{Exception} data.Array t
data.Array.toList : data.Array a -> [a]
data.Array.unsafeAt : Nat -> data.Array a -> a
structural type data.Bag a
data.Bag.* : Bag a -> Bag b -> Bag (a, b)
data.Bag.+ : Bag a -> Bag a -> Bag a
data.Bag.== : Bag a -> Bag a -> Boolean
data.Bag.add : k -> Bag k -> Bag k
data.Bag.addAll : Bag a -> Bag a -> Bag a
data.Bag.addN : Nat -> k -> Bag k -> Bag k
data.Bag.all : (a ->{g} Boolean) -> Bag a ->{g} Boolean
data.Bag.any : (a ->{g} Boolean) -> Bag a ->{g} Boolean
data.Bag.contains : Bag a -> a -> Boolean
data.Bag.convolve : (i ->{g} a ->{g} t) -> Bag i -> Bag a ->{g} Bag t
data.Bag.count : a -> Bag a -> Nat
data.Bag.counts : Bag a -> Map a Nat
data.Bag.difference : Bag a -> Bag a -> Bag a
data.Bag.elementOf : a -> Bag a -> Boolean
data.Bag.empty : Bag a
data.Bag.equals : Bag a -> Bag a -> Boolean
data.Bag.filter : (i ->{e} Boolean) -> Bag i ->{e} Bag i
data.Bag.flatMap : (a ->{e} Bag b) -> Bag a ->{e} Bag b
data.Bag.foldLeft : (b ->{g} a ->{g} b) -> b -> Bag a ->{g} b
data.Bag.foldRight : (a ->{g} b ->{g} b) -> b -> Bag a ->{g} b
data.Bag.from : Bag a -> Bag a -> Boolean
data.Bag.fromList : [k] -> Bag k
data.Bag.fromMap : Map k Nat -> Bag k
data.Bag.fromOccurrenceList : [(a, Nat)] -> Bag a
data.Bag.fromText : Text -> Bag Char
data.Bag.internal.compare : (Nat ->{e} Nat ->{e} Boolean) -> Bag a -> Bag a ->{e} Boolean
data.Bag.internal.MkBag : Map a Nat -> Bag a
data.Bag.internal.normalize : Bag a -> Bag a
data.Bag.intersect : Bag a -> Bag a -> Bag a
data.Bag.isEmpty : Bag a -> Boolean
data.Bag.map : (a ->{g} b) -> Bag a ->{g} Bag b
data.Bag.mapCounts : (Nat ->{e} Nat) -> Bag a ->{e} Bag a
data.Bag.modify : ((a, Nat) ->{e} (b, Nat)) -> Bag a ->{e} Bag b
data.Bag.none : (a ->{g} Boolean) -> Bag a ->{g} Boolean
data.Bag.nth : Nat -> Bag a -> Optional a
data.Bag.occurrenceList : Bag a -> [(a, Nat)]
data.Bag.product : Bag a -> Bag b -> Bag (a, b)
data.Bag.randomChoice : Bag a ->{Exception, Random} a
data.Bag.remove : k -> Bag k -> Bag k
data.Bag.removeAll : k -> Bag k -> Bag k
data.Bag.removeN : Nat -> k -> Bag k -> Bag k
data.Bag.removeWhere : (a ->{e} Boolean) -> Bag a ->{e} Bag a
data.Bag.scale : Nat -> Bag a -> Bag a
data.Bag.similarity : Bag a -> Bag a -> Float
data.Bag.singleton : k -> Bag k
data.Bag.size : Bag a -> Nat
data.Bag.subbag : Bag a -> Bag a -> Boolean
data.Bag.superbag : Bag a -> Bag a -> Boolean
data.Bag.toList : Bag a -> [a]
data.Bag.toSet : Bag a -> Set a
data.Bag.toText : Bag Char -> Text
data.Bag.traverse : (a ->{e} b) -> Bag a ->{e} Bag b
data.Bag.union : Bag a -> Bag a -> Bag a
structural type data.ByteArray
data.ByteArray.++ : data.ByteArray -> data.ByteArray -> data.ByteArray
data.ByteArray.append : data.ByteArray -> data.ByteArray -> data.ByteArray
data.ByteArray.BArr : Nat -> Nat -> data.ByteArray.Raw -> data.ByteArray
data.ByteArray.base32Hex : data.ByteArray -> Text
data.ByteArray.drop : Nat -> data.ByteArray -> data.ByteArray
data.ByteArray.find : (Nat ->{g} Boolean) -> data.ByteArray ->{g, Exception} Optional Nat
data.ByteArray.findLast : (Nat ->{g} Boolean)
                          -> data.ByteArray
                          ->{g, Exception} Optional Nat
data.ByteArray.firstIndexOf : (Nat ->{g} Boolean)
                              -> data.ByteArray
                              ->{g, Exception} Optional Nat
data.ByteArray.fromBytes : Bytes -> data.ByteArray
data.ByteArray.fromList : [Nat] -> data.ByteArray
data.ByteArray.join : [data.ByteArray] -> data.ByteArray
data.ByteArray.lastIndexOf : (Nat ->{g} Boolean)
                             -> data.ByteArray
                             ->{g, Exception} Optional Nat
data.ByteArray.new! : Nat
                      -> (∀ s. mutable.ByteArray (Scope s) ->{g, Exception, Scope s} ())
                      ->{g} data.ByteArray
data.ByteArray.randomChoice : data.ByteArray ->{Exception, Random} Nat
builtin type data.ByteArray.Raw
data.ByteArray.Raw.base32Hex : data.ByteArray.Raw -> Nat -> Nat -> Text
data.ByteArray.Raw.copyTo! : mutable.ByteArray.Raw g
                             -> Nat
                             -> data.ByteArray.Raw
                             -> Nat
                             -> Nat
                             ->{g, Exception} ()
data.ByteArray.Raw.fromBytes : Bytes -> data.ByteArray.Raw
data.ByteArray.Raw.new! : Nat
                          -> (∀ s.
                            mutable.ByteArray.Raw (Scope s) ->{g, Exception, Scope s} ())
                          ->{g} data.ByteArray.Raw
data.ByteArray.Raw.read16be : data.ByteArray.Raw -> Nat ->{Exception} Nat
data.ByteArray.Raw.read24be : data.ByteArray.Raw -> Nat ->{Exception} Nat
data.ByteArray.Raw.read32be : data.ByteArray.Raw -> Nat ->{Exception} Nat
data.ByteArray.Raw.read40be : data.ByteArray.Raw -> Nat ->{Exception} Nat
data.ByteArray.Raw.read64be : data.ByteArray.Raw -> Nat ->{Exception} Nat
data.ByteArray.Raw.read8 : data.ByteArray.Raw -> Nat ->{Exception} Nat
data.ByteArray.Raw.size : data.ByteArray.Raw -> Nat
data.ByteArray.Raw.toByteList : data.ByteArray.Raw -> [Nat]
data.ByteArray.Raw.toBytes : data.ByteArray.Raw -> Bytes
data.ByteArray.read16be : data.ByteArray -> Nat ->{Exception} Nat
data.ByteArray.read32be : data.ByteArray -> Nat ->{Exception} Nat
data.ByteArray.read64be : data.ByteArray -> Nat ->{Exception} Nat
data.ByteArray.read8 : data.ByteArray -> Nat ->{Exception} Nat
data.ByteArray.size : data.ByteArray -> Nat
data.ByteArray.slice : Nat -> Nat -> data.ByteArray ->{Exception} data.ByteArray
data.ByteArray.take : Nat -> data.ByteArray -> data.ByteArray
data.ByteArray.toList : data.ByteArray -> [Nat]
data.ByteArray.write8 : data.ByteArray -> Nat -> Nat ->{Exception} data.ByteArray
structural type data.deprecated.Heap k v
data.deprecated.Heap.breakOffMax : Heap k v -> (k, v, Optional (Heap k v))
data.deprecated.Heap.children : Heap k v -> [Heap k v]
data.deprecated.Heap.fromKeys : [a] -> Optional (Heap a a)
data.deprecated.Heap.fromList : List.Nonempty (k, v) -> Heap k v
data.deprecated.Heap.Heap : Nat -> k -> v -> [Heap k v] -> Heap k v
data.deprecated.Heap.insert : k -> v -> Heap k v -> Heap k v
data.deprecated.Heap.max : Heap k v -> (k, v)
data.deprecated.Heap.maxKey : Heap k v -> k
data.deprecated.Heap.mayFromList : [(k, v)] -> Optional (Heap k v)
data.deprecated.Heap.pop : Heap k v -> Optional (Heap k v)
data.deprecated.Heap.singleton : k -> v -> Heap k v
data.deprecated.Heap.size : Heap k v -> Nat
data.deprecated.Heap.sort : [a] -> [a]
data.deprecated.Heap.sortDescending : [a] -> [a]
data.deprecated.Heap.take : Nat -> Heap k v -> [(k, v)]
data.deprecated.Heap.toList : Heap k v -> List.Nonempty (k, v)
data.deprecated.Heap.union : Heap k v -> Heap k v -> Heap k v
structural type data.deprecated.Weighted a
data.deprecated.Weighted.<|> : Weighted a -> Weighted a -> Weighted a
data.deprecated.Weighted.append : Weighted a -> Weighted a -> Weighted a
data.deprecated.Weighted.append.examples.ex : [Nat]
data.deprecated.Weighted.dedupe : Weighted a -> Weighted a
data.deprecated.Weighted.drop : Nat -> Weighted a -> Weighted a
data.deprecated.Weighted.Fail : Weighted a
data.deprecated.Weighted.filter : (a -> Boolean) -> Weighted a -> Weighted a
data.deprecated.Weighted.flatMap : (a -> Weighted b) -> Weighted a -> Weighted b
data.deprecated.Weighted.floats : Weighted Float
data.deprecated.Weighted.fromList : [a] -> Weighted a
data.deprecated.Weighted.ints : Weighted Int
data.deprecated.Weighted.lists : Weighted a -> Weighted [a]
data.deprecated.Weighted.map : (a -> b) -> Weighted a -> Weighted b
data.deprecated.Weighted.mergeWith : (a -> b -> c)
                                     -> Weighted a
                                     -> Weighted b
                                     -> Weighted c
data.deprecated.Weighted.nats : Weighted Nat
data.deprecated.Weighted.natsInOrder : Weighted Nat
data.deprecated.Weighted.normalFloats : Weighted Float
data.deprecated.Weighted.sample : Nat -> Weighted a -> [a]
data.deprecated.Weighted.take : Nat -> Weighted a -> Weighted a
data.deprecated.Weighted.weight : Nat ->{e} '{e} Weighted a ->{e} Weighted a
data.deprecated.Weighted.Weight : Nat -> 'Weighted a -> Weighted a
data.deprecated.Weighted.yield : a -> Weighted a
data.deprecated.Weighted.Yield : a -> Weighted a -> Weighted a
type data.Graph v
data.Graph.AdjLists : data.Array.Raw [Nat] -> data.Array.Raw v -> Graph v
data.Graph.build : [(v, k, [k])] -> Graph v
data.Graph.dfs : Graph v -> [Nat] ->{Exception} [RoseTree Nat]
data.Graph.dfs.crawl : Graph v
                       -> NatSet
                       -> [RoseTree Nat]
                       -> Nat
                       ->{Exception} (NatSet, [RoseTree Nat])
data.Graph.dfs.crawls : Graph v -> NatSet -> [Nat] ->{Exception} (NatSet, [RoseTree Nat])
data.Graph.edgeCount : Graph v -> Nat
data.Graph.edges : Graph v -> Nat ->{Exception} [Nat]
data.Graph.isReachable : Graph v -> Nat -> Nat ->{Exception} Boolean
data.Graph.reverse : Graph v ->{Exception} Graph v
type data.Graph.SCC v
data.Graph.SCC.Acyclic : v -> SCC v
data.Graph.SCC.add : a -> SCC a -> SCC a
data.Graph.SCC.augment : Boolean -> a -> Optional (SCC a) -> SCC a
data.Graph.SCC.Cyclic : [v] -> SCC v
data.Graph.SCC.flatten : SCC v -> [v]
data.Graph.SCC.map : (a ->{g1} b) -> SCC a ->{g1} SCC b
data.Graph.sccs : Graph v -> [SCC v]
data.Graph.sccs.classify : Graph v -> RoseTree Nat ->{Exception} SCC Nat
data.Graph.sccs.crawl : Graph v -> NatSet -> [Nat] -> Nat ->{Exception} (NatSet, [Nat])
data.Graph.sccs.crawls : Graph v -> (NatSet, [Nat]) -> [Nat] ->{Exception} (NatSet, [Nat])
data.Graph.stronglyConnectedComponents : [(v, k, [k])] -> [SCC v]
data.Graph.topSort : Graph v ->{Abort} [v]
data.Graph.topSort.crawl : Graph v
                           -> NatSet
                           -> (NatSet, [v])
                           -> Nat
                           ->{Exception, Abort} (NatSet, [v])
data.Graph.topSort.crawls : Graph v
                            -> NatSet
                            -> (NatSet, [v])
                            -> [Nat]
                            ->{Exception, Abort} (NatSet, [v])
data.Graph.vertex : Graph v -> Nat ->{Exception} v
data.Graph.vertexCount : Graph v -> Nat
data.Graph.vertexNum : Graph v -> v ->{Exception} Optional Nat
structural type data.Id a
data.Id.apply : Id i -> Id (i ->{g} t) ->{g} Id t
data.Id.flatMap : (i ->{g} o) -> Id i ->{g} o
data.Id.Id : a -> Id a
data.Id.map : (i ->{g} o) -> Id i ->{g} Id o
builtin type data.List
data.List.++ : [a] -> [a] -> [a]
data.List.+: : a -> [a] -> [a]
data.List.:+ : [a] -> a -> [a]
data.List.align : [a] -> [b] -> [OneOrBoth a b]
data.List.alignWith : (OneOrBoth a b ->{g} c) -> [a] -> [b] ->{g} [c]
data.List.all : (a ->{e} Boolean) -> [a] ->{e} Boolean
data.List.allPairs : [a] -> [(a, a)]
data.List.any : (a ->{e} Boolean) -> [a] ->{e} Boolean
data.List.anyIndexOf : a -> [a] -> Optional Nat
data.List.anyIndexOf.evaluated.empty : Optional Nat
data.List.anyIndexOf.evaluated.lower : Optional Nat
data.List.anyIndexOf.evaluated.notSorted : Optional Nat
data.List.anyIndexOf.evaluated.sorted : Optional Nat
data.List.anyIndexOf.evaluated.upper : Optional Nat
data.List.anyIndexOf.examples.input.lower : [Nat]
data.List.anyIndexOf.examples.input.notSorted : [Nat]
data.List.anyIndexOf.examples.input.sorted : [Nat]
data.List.anyIndexOf.examples.input.upper : [Nat]
data.List.apply : [a ->{e} b] -> [a] ->{e} [b]
data.List.at : Nat -> [a] -> Optional a
data.List.at! : Nat -> [a] ->{Abort} a
data.List.chunk : Nat -> [a] -> [[a]]
data.List.combinations : Nat -> [a] -> [[a]]
data.List.compare : (a ->{f} b ->{g} Ordering) -> [a] -> [b] ->{f, g} Ordering
data.List.concatOptional : Optional [a] -> [a]
data.List.cons : a -> [a] -> [a]
data.List.contains : a -> [a] -> Boolean
data.List.containsNot : a -> [a] -> Boolean
data.List.count : (a ->{e} Boolean) -> [a] ->{e} Nat
data.List.countElement : a -> [a] -> Nat
data.List.deleteAt : Nat -> [a] -> [a]
data.List.deleteFirst : (a ->{g} Boolean) -> [a] ->{g} [a]
data.List.deprecated.lubIndexOf : a -> [a] -> Nat
data.List.deprecated.lubIndexOf.evaluated.empty : Nat
data.List.deprecated.lubIndexOf.evaluated.notSorted : Nat
data.List.deprecated.lubIndexOf.evaluated.sorted : Nat
data.List.deprecated.lubIndexOf' : a -> Nat -> [a] -> Nat
data.List.diagonal : [[a]] -> [a]
data.List.diagonals : [[a]] -> [[a]]
data.List.distinct : [a] -> [a]
data.List.distinctBy : (a ->{g} b) -> [a] ->{g} [a]
data.List.drop : Nat -> [a] -> [a]
data.List.dropLast : [a] -> [a]
data.List.dropRight : Nat -> [a] -> [a]
data.List.dropRightWhile : (a ->{g} Boolean) -> [a] ->{g} [a]
data.List.dropUntil : (a ->{e} Boolean) -> [a] ->{e} [a]
data.List.dropWhile : (a ->{e} Boolean) -> [a] ->{e} [a]
data.List.dropWhile.examples.ex1 : [Nat]
data.List.empty : [a]
data.List.equals : (a ->{f} b ->{g} Boolean) -> [a] -> [b] ->{f, g} Boolean
data.List.every : Nat -> [a] -> [a]
data.List.fill : Nat -> a -> [a]
data.List.fill.examples.ex1 : [Text]
data.List.fill' : Nat -> '{g} a ->{g} [a]
data.List.filter : (a ->{g} Boolean) -> [a] ->{g} [a]
data.List.filterMap : (a ->{e} Optional b) -> [a] ->{e} [b]
data.List.filterMap.examples.ex1 : [Nat]
data.List.find : (a ->{g} Boolean) -> [a] ->{g} Optional a
data.List.find! : (a ->{g} Boolean) -> [a] ->{g, Abort} a
data.List.findFirstIndex : (a ->{e} Boolean) -> [a] ->{e} Optional Nat
data.List.findLastIndex : (a ->{e} Boolean) -> [a] ->{e} Optional Nat
data.List.findMap : (a ->{g} Optional b) -> [a] ->{g} Optional b
data.List.firstIndexOf : a -> [a] -> Optional Nat
data.List.flatMap : (a ->{e} [b]) -> [a] ->{e} [b]
data.List.flatMap.examples.ex1 : [Nat]
data.List.flatMap.examples.ex2 : [Nat]
data.List.flatMapRight : (a ->{e} [b]) -> [a] ->{e} [b]
data.List.flatMapRight.examples.ex1 : [Nat]
data.List.flatMapRight.examples.ex2 : [Nat]
data.List.foldDelimited : (b ->{g2} b ->{g1} b)
                          -> (a ->{g} b)
                          -> b
                          -> b
                          -> b
                          -> [a]
                          ->{g, g1, g2} b
data.List.foldLeft : (b ->{g} a ->{g} b) -> b -> [a] ->{g} b
data.List.foldMap : (a ->{e} b) -> (b ->{e} b ->{e} b) -> b -> [a] ->{e} b
data.List.foldRight : (a ->{e} b ->{e} b) -> b -> [a] ->{e} b
data.List.foreach : (a ->{g} ()) -> [a] ->{g} ()
data.List.foreach.deprecated : (a ->{g} b) -> [a] ->{g} ()
data.List.foreach.flipped : [a] -> (a ->{g} ()) ->{g} ()
data.List.foreach.flipped.deprecated : [a] -> (a ->{g} b) ->{g} ()
data.List.groupBy : (v ->{e} k) -> [v] ->{e} Map k (List.Nonempty v)
data.List.groupBy.examples.ex1 : [[Nat]]
data.List.groupBy.examples.ex2 : [[Nat]]
data.List.groupBy.reversed : (v ->{e} k) -> [v] ->{e} Map k (List.Nonempty v)
data.List.groupConsecutive : [a] -> [List.Nonempty a]
data.List.groupMap : (a ->{e} k) -> (a ->{e} v) -> [a] ->{e} Map k (List.Nonempty v)
data.List.groupMapReduce : (a ->{e} k)
                           -> (a ->{e} v)
                           -> (v ->{e} v ->{e} v)
                           -> [a]
                           ->{e} Map k v
data.List.groupSublistsBy : (a ->{e} a ->{e} Boolean) -> [a] ->{e} [List.Nonempty a]
data.List.groupSublistsBy.examples.ex1 : [[Nat]]
data.List.groupSublistsBy.examples.ex2 : [[Nat]]
data.List.groupWith : (a ->{e} a ->{f} Boolean) -> [a] ->{e, f} [[a]]
data.List.halve : [a] -> ([a], [a])
data.List.head : [a] -> Optional a
data.List.head.examples.evaluated.elems : Optional Nat
data.List.head.examples.evaluated.empty : Optional a
data.List.head.examples.evaluated.single : Optional Nat
data.List.indexed : [a] -> [(a, Nat)]
data.List.indexOfSublist : [a] -> [a] -> Optional Nat
data.List.init : [a] -> Optional [a]
data.List.initialize : Nat -> (Nat ->{e} a) ->{e} [a]
data.List.initialize.examples.ex1 : [Nat]
data.List.inits : [a] -> [[a]]
data.List.insertAfter : (a ->{e} a ->{f} Boolean) -> a -> [a] ->{e, f} [a]
data.List.insertAt : Nat -> a -> [a] -> [a]
data.List.insertSortedDistinct : a -> [a] -> [a]
data.List.intercalate : [a] -> [[a]] -> [a]
data.List.interleave : [a] -> [a] -> [a]
data.List.intersperse : a -> [a] -> [a]
data.List.isEmpty : [a] -> Boolean
data.List.isInfixOf : [a] -> [a] -> Boolean
data.List.isPrefixOf : [a] -> [a] -> Boolean
data.List.isSortedBy : (a -> a ->{e} Boolean) -> [a] ->{e} Boolean
data.List.isSuffixOf : [a] -> [a] -> Boolean
data.List.iterate : (a ->{Abort} a) -> a -> [a]
data.List.join : [[a]] -> [a]
data.List.last : [a] -> Optional a
data.List.last.examples.elems : [Nat]
data.List.last.examples.empty : [a]
data.List.last.examples.evaluated.elems : Optional Nat
data.List.last.examples.evaluated.empty : Optional a
data.List.last.examples.evaluated.single : Optional Nat
data.List.last.examples.single : [Nat]
data.List.lefts : [Either a b] -> [a]
data.List.map : (a ->{g} b) -> [a] ->{g} [b]
data.List.map2 : (a ->{e} b ->{e} c) -> [a] -> [b] ->{e} [c]
data.List.mapIndexed : (Nat ->{g} a ->{g} b) -> [a] ->{g} [b]
data.List.mapRight : (a ->{g} b) -> [a] ->{g} [b]
data.List.maximum : [a] -> Optional a
data.List.maximumBy : (a ->{e} a ->{e} Ordering) -> [a] ->{e} Optional a
data.List.maximumOn : (b ->{f} b ->{g} Ordering)
                      -> (a ->{e} b)
                      -> [a]
                      ->{e, f, g} Optional a
data.List.mayNonempty : [a] -> Optional (List.Nonempty a)
data.List.minimum : [a] -> Optional a
data.List.minimumBy : (a ->{e} a ->{e} Ordering) -> [a] ->{e} Optional a
data.List.modifyAt : Nat -> (a ->{g} a) -> [a] ->{g} Optional [a]
data.List.mostFrequent : [a] -> Optional a
data.List.none : (i ->{e} Boolean) -> [i] ->{e} Boolean
structural type data.List.Nonempty a
data.List.nonempty : [a] ->{Abort} List.Nonempty a
data.List.Nonempty.++ : List.Nonempty a -> List.Nonempty a -> List.Nonempty a
data.List.Nonempty.+| : a -> [a] -> List.Nonempty a
data.List.Nonempty.align : List.Nonempty a
                           -> List.Nonempty b
                           -> List.Nonempty (OneOrBoth a b)
data.List.Nonempty.alignWith : (OneOrBoth a b ->{g} c)
                               -> List.Nonempty a
                               -> List.Nonempty b
                               ->{g} List.Nonempty c
data.List.Nonempty.append : List.Nonempty a -> List.Nonempty a -> List.Nonempty a
data.List.Nonempty.appendList : List.Nonempty a -> [a] -> List.Nonempty a
data.List.Nonempty.at : Nat -> List.Nonempty a -> Optional a
data.List.Nonempty.cons : a -> List.Nonempty a -> List.Nonempty a
data.List.Nonempty.examples.construction.ex1 : List.Nonempty Nat
data.List.Nonempty.examples.construction.ex2 : Optional (List.Nonempty Nat)
data.List.Nonempty.filterMap : (a ->{g} Optional b)
                               -> List.Nonempty a
                               ->{g} Optional (List.Nonempty b)
data.List.Nonempty.flatMap : (a ->{e} List.Nonempty b)
                             -> List.Nonempty a
                             ->{e} List.Nonempty b
data.List.Nonempty.foldLeft : (b ->{e} a ->{e} b) -> b -> List.Nonempty a ->{e} b
data.List.Nonempty.foldMap : (b ->{e} b ->{e} b) -> (a ->{e} b) -> List.Nonempty a ->{e} b
data.List.Nonempty.foldMap.examples.ex1 : Text
data.List.Nonempty.foldRight : (a ->{e} b ->{e} b) -> b -> List.Nonempty a ->{e} b
data.List.Nonempty.head : List.Nonempty a -> a
data.List.Nonempty.init : List.Nonempty a -> [a]
data.List.Nonempty.join : List.Nonempty (List.Nonempty a) -> List.Nonempty a
data.List.Nonempty.join.examples.ex1 : List.Nonempty Nat
data.List.Nonempty.last : List.Nonempty a -> a
data.List.Nonempty.map : (a ->{e} b) -> List.Nonempty a ->{e} List.Nonempty b
data.List.Nonempty.maximum : List.Nonempty a -> a
data.List.Nonempty.maximumBy : (a ->{e} a ->{e} Ordering) -> List.Nonempty a ->{e} a
data.List.Nonempty.minimum : List.Nonempty a -> a
data.List.Nonempty.minimumBy : (a ->{e} a ->{e} Ordering) -> List.Nonempty a ->{e} a
data.List.Nonempty.Nonempty : a -> [a] -> List.Nonempty a
data.List.Nonempty.prependList : [a] -> List.Nonempty a -> List.Nonempty a
data.List.Nonempty.randomChoice : List.Nonempty a ->{Random} a
data.List.Nonempty.reduceLeft : (a ->{e} a ->{e} a) -> List.Nonempty a ->{e} a
data.List.Nonempty.reduceRight : (a ->{e} a ->{e} a) -> List.Nonempty a ->{e} a
data.List.Nonempty.reverse : List.Nonempty a -> List.Nonempty a
data.List.Nonempty.scanLeft : (a ->{e} a ->{e} a) -> List.Nonempty a ->{e} List.Nonempty a
data.List.Nonempty.scanRight : (a ->{e} a ->{e} a) -> List.Nonempty a ->{e} List.Nonempty a
data.List.Nonempty.sequenceOptional : List.Nonempty (Optional a)
                                      -> Optional (List.Nonempty a)
data.List.Nonempty.singleton : a -> List.Nonempty a
data.List.Nonempty.size : List.Nonempty a -> Nat
data.List.Nonempty.snoc : List.Nonempty a -> a -> List.Nonempty a
data.List.Nonempty.somes : List.Nonempty (Optional a) -> Optional (List.Nonempty a)
data.List.Nonempty.tail : List.Nonempty a -> [a]
data.List.Nonempty.toList : List.Nonempty a -> [a]
data.List.Nonempty.toSet : List.Nonempty a -> Set a
data.List.Nonempty.traverseOptional : (a ->{g} Optional b)
                                      -> List.Nonempty a
                                      ->{g} Optional (List.Nonempty b)
data.List.Nonempty.zipWith : (a ->{g1} b ->{g} c)
                             -> List.Nonempty a
                             -> List.Nonempty b
                             ->{g1, g} List.Nonempty c
data.List.Nonempty.|+ : [a] -> a -> List.Nonempty a
data.List.nonEmptySubsequences : [a] -> [List.Nonempty a]
data.List.of : x -> Nat -> [x]
data.List.partition : (a ->{g} Boolean) -> [a] ->{g} ([a], [a])
data.List.partitionBy : (a ->{g} Boolean) -> [a] ->{g} [List.Nonempty a]
data.List.partitionEithers : [Either a b] -> ([a], [b])
data.List.partitionMap : (a ->{g} Either b c) -> [a] ->{g} ([b], [c])
data.List.powerslice : [a] -> [[a]]
data.List.powerslice.examples.ex1 : [[Nat]]
data.List.powerslice.examples.ex2 : [[a]]
data.List.randomChoice : [a] ->{Exception, Random} a
data.List.range : Nat -> Nat -> [Nat]
data.List.rangeClosed : Nat -> Nat -> [Nat]
data.List.replace : Nat -> a -> [a] -> [a]
data.List.replicate : Nat -> '{e} a ->{e} [a]
data.List.reverse : [a] -> [a]
data.List.rights : [Either a b] -> [b]
data.List.scanLeft : (b ->{e} a ->{e} b) -> b -> [a] ->{e} List.Nonempty b
data.List.scanLeft.examples.ex1 : List.Nonempty Nat
data.List.scanLeft.examples.ex2 : List.Nonempty Nat
data.List.scanLeft.examples.ex3 : List.Nonempty Nat
data.List.scanLeft.examples.ex4 : List.Nonempty Nat
data.List.scanLeft.examples.ex5 : List.Nonempty Nat
data.List.scanRight : (a ->{e} b ->{e} b) -> b -> [a] ->{e} List.Nonempty b
data.List.scanRight.examples.ex1 : List.Nonempty Nat
data.List.scanRight.examples.ex2 : List.Nonempty Nat
data.List.scanRight.examples.ex3 : List.Nonempty Nat
data.List.scanRight.examples.ex4 : List.Nonempty Boolean
data.List.scanRight.examples.ex5 : List.Nonempty Nat
data.List.sequenceOptional : [Optional a] -> Optional [a]
data.List.singleton : a -> [a]
data.List.size : [a] -> Nat
data.List.skip : Nat -> [a] -> [a]
data.List.slice : Nat -> Nat -> [a] -> [a]
data.List.slidingPairs : [a] -> [(a, a)]
data.List.slidingWindow : Nat -> [a] -> [[a]]
data.List.snoc : [a] -> a -> [a]
data.List.somes : [Optional a] -> [a]
data.List.sort : [a] -> [a]
data.List.sortBy : (a ->{e} b) -> [a] ->{e} [a]
data.List.sortWith : (a -> a ->{e} Boolean) -> [a] ->{e} [a]
data.List.span : (a ->{e} Boolean) -> [a] ->{e} ([a], [a])
data.List.split : (a ->{e} Boolean) -> [a] ->{e} [[a]]
data.List.splitAt : Nat -> [a] -> ([a], [a])
data.List.stripPrefix : [a] -> [a] -> Optional [a]
data.List.subsequences : [a] -> [[a]]
data.List.tail : [a] -> Optional [a]
data.List.tails : [a] -> [[a]]
data.List.take : Nat -> [a] -> [a]
data.List.takeRight : Nat -> [a] -> [a]
data.List.takeUntil : (a ->{e} Boolean) -> [a] ->{e} [a]
data.List.takeWhile : (a ->{e} Boolean) -> [a] ->{e} [a]
data.List.toBag : [k] -> Bag k
data.List.toMap : [(k, v)] -> Map k v
data.List.toSet : [k] -> Set k
data.List.toStream : [a] ->{Stream a} ()
data.List.toText : (a ->{g} Text) -> [a] ->{g} Text
data.List.transpose : [[a]] -> [[a]]
data.List.traverseOptional : (a ->{g} Optional b) -> [a] ->{g} Optional [b]
data.List.uncollate : [a] -> ([a], [a])
data.List.uncons : [a] -> Optional (a, [a])
                                      -> Optional (i1, i)
                                      ->{g1, g} [elem]
data.List.unfold : s -> (s ->{g} Optional (a, s)) ->{g} [a]
data.List.unsafeAt : Nat -> [a] -> a
data.List.unsnoc : [a] -> Optional ([a], a)
data.List.unzip : [(a, b)] -> ([a], [b])
data.List.updateAt : (a ->{g1} a) -> Nat -> [a] ->{g1} [a]
data.List.zip : [a] -> [b] -> [(a, b)]
data.List.zipWith : (a ->{g} b ->{g} c) -> [a] -> [b] ->{g} [c]
type data.Map k v
data.Map.== : Map k v -> Map k v -> Boolean
data.Map.adjust : (v ->{e} v) -> k -> Map k v ->{e} Map k v
data.Map.adjustWithKey : (k ->{e} v ->{e} v) -> k -> Map k v ->{e} Map k v
data.Map.align : Map k a -> Map k b -> Map k (OneOrBoth a b)
data.Map.alignWith : (OneOrBoth a b ->{g} c) -> Map k a -> Map k b ->{g} Map k c
data.Map.alignWithKey : (k ->{e} OneOrBoth a b ->{f} c)
                        -> Map k a
                        -> Map k b
                        ->{e, f} Map k c
data.Map.alter : (Optional v ->{e} Optional v) -> k -> Map k v ->{e} Map k v
data.Map.breakOffMax : Map k v -> Optional ((k, v), Map k v)
data.Map.breakOffMax! : Map k v ->{Abort} ((k, v), Map k v)
data.Map.breakOffMin : Map k v -> Optional ((k, v), Map k v)
data.Map.breakOffMin! : Map k v ->{Abort} ((k, v), Map k v)
data.Map.contains : k -> Map k v -> Boolean
data.Map.delete : k -> Map k v -> Map k v
data.Map.difference : Map k a -> Map k b -> Map k a
data.Map.empty : Map k v
data.Map.filter : (v ->{g} Boolean) -> Map k v ->{g} Map k v
data.Map.filterAlignWithKey : (k ->{e} OneOrBoth a b ->{f} Optional c)
                              -> Map k a
                              -> Map k b
                              ->{e, f} Map k c
data.Map.filterKeys : (k ->{g} Boolean) -> Map k v ->{g} Map k v
data.Map.filterWithKey : (k ->{e} a ->{f} Boolean) -> Map k a ->{e, f} Map k a
data.Map.foldLeft : (a ->{e} b ->{e} a) -> a -> Map k b ->{e} a
data.Map.foldLeftWithKey : (a ->{e} k ->{e} b ->{e} a) -> a -> Map k b ->{e} a
data.Map.foldMap : (a ->{e} a ->{e} a) -> (k ->{e} v ->{e} a) -> Map k v ->{e} Optional a
data.Map.foldRight : (a ->{e} b ->{e} b) -> b -> Map k a ->{e} b
data.Map.foldRightWithKey : (k ->{e} a ->{e} b ->{e} b) -> b -> Map k a ->{e} b
data.Map.foreach : Map k v -> (k ->{e} v ->{e} ()) ->{e} ()
data.Map.fromList : [(k, v)] -> Map k v
data.Map.fromListWith : (v ->{e} v ->{e} v) -> [(k, v)] ->{e} Map k v
data.Map.fromListWithKey : (k ->{e} v ->{e} v ->{e} v) -> [(k, v)] ->{e} Map k v
data.Map.get : k -> Map k v -> Optional v
data.Map.getMax : Map k v -> Optional (k, v)
data.Map.getMin : Map k v -> Optional (k, v)
data.Map.getOrAbort : k -> Map k v ->{Abort} v
data.Map.getOrElse : v -> k -> Map k v -> v
data.Map.getOrThrow : e -> k -> Map k v ->{Throw e} v
data.Map.gt : k -> Map k v -> Map k v
data.Map.gteq : k -> Map k v -> Map k v
data.Map.insert : k -> v -> Map k v -> Map k v
data.Map.insertIfMissing : k -> '{g} v -> Map k v ->{g} (Map k v, Either v v)
data.Map.insertNonempty : k -> v -> Map k v -> Map.Nonempty k v
data.Map.internal.balance : k -> v -> Map k v -> Map k v -> Map k v
data.Map.internal.balanceL : k -> v -> Map k v -> Map k v -> Map k v
data.Map.internal.balanceR : k -> v -> Map k v -> Map k v -> Map k v
data.Map.internal.bin : k -> v -> Map k v -> Map k v -> Map k v
data.Map.internal.Bin : Nat -> k -> v -> Map k v -> Map k v -> Map k v
data.Map.internal.delta : Nat
data.Map.internal.glue : Map k v -> Map k v -> Map k v
data.Map.internal.link : k -> v -> Map k v -> Map k v -> Map k v
data.Map.internal.link2 : Map k v -> Map k v -> Map k v
type data.Map.internal.MaxView k v
data.Map.internal.MaxView.MaxView : k -> v -> Map k v -> MaxView k v
data.Map.internal.maxViewSure : k -> v -> Map k v -> Map k v -> MaxView k v
type data.Map.internal.MinView k v
data.Map.internal.MinView.MinView : k -> v -> Map k v -> MinView k v
data.Map.internal.minViewSure : k -> v -> Map k v -> Map k v -> MinView k v
data.Map.internal.putMax : k -> v -> Map k v -> Map k v
data.Map.internal.putMin : k -> v -> Map k v -> Map k v
data.Map.internal.putWithKeyR : (k ->{e} v ->{e} v ->{e} v)
                                -> k
                                -> v
                                -> Map k v
                                ->{e} Map k v
data.Map.internal.putWithR : (v ->{e} v ->{e} v) -> k -> v -> Map k v ->{e} Map k v
data.Map.internal.ratio : Nat
data.Map.internal.splitLookup : k -> Map k v -> (Map k v, Optional v, Map k v)
data.Map.internal.Tip : Map k v
data.Map.intersect : Map k a -> Map k b -> Map k a
data.Map.intersectWith : (a ->{e} b ->{e} c) -> Map k a -> Map k b ->{e} Map k c
data.Map.intersectWithKey : (k ->{e} a ->{e} b ->{e} c) -> Map k a -> Map k b ->{e} Map k c
data.Map.isEmpty : Map k v -> Boolean
data.Map.keys : Map k a -> [k]
data.Map.lookup : k -> Map k v -> Optional v
data.Map.lt : k -> Map k v -> Map k v
data.Map.lteq : k -> Map k v -> Map k v
data.Map.map : (a ->{e} b) -> Map k a ->{e} Map k b
data.Map.mapKeys : (k1 ->{g} k2) -> Map k1 v ->{g} Map k2 v
data.Map.mapKeysWith : (v ->{g} v ->{g} v) -> (k1 ->{g} k2) -> Map k1 v ->{g} Map k2 v
data.Map.mapOptional : (a ->{f} Optional b) -> Map k a ->{f} Map k b
data.Map.mapOptionalWithKey : (k ->{e} a ->{f} Optional b) -> Map k a ->{e, f} Map k b
data.Map.mapWithKey : (k ->{e} a ->{e} b) -> Map k a ->{e} Map k b
data.Map.mergeWith : (OneOrBoth a b ->{g} Optional c) -> Map k a -> Map k b ->{g} Map k c
data.Map.mergeWithKey : (k ->{e} OneOrBoth a b ->{f} Optional c)
                        -> Map k a
                        -> Map k b
                        ->{e, f} Map k c
type data.Map.Nonempty k v
data.Map.Nonempty.== : Map.Nonempty k v -> Map.Nonempty k v -> Boolean
data.Map.Nonempty.adjust : (v ->{e} v) -> k -> Map.Nonempty k v ->{e} Map.Nonempty k v
data.Map.Nonempty.adjustWithKey : (k ->{e} v ->{e} v)
                                  -> k
                                  -> Map.Nonempty k v
                                  ->{e} Map.Nonempty k v
data.Map.Nonempty.align : Map.Nonempty k a
                          -> Map.Nonempty k b
                          -> Map.Nonempty k (OneOrBoth a b)
data.Map.Nonempty.alignWith : (OneOrBoth a b ->{g} c)
                              -> Map.Nonempty k a
                              -> Map.Nonempty k b
                              ->{g} Map.Nonempty k c
data.Map.Nonempty.alignWithKey : (k ->{e} OneOrBoth a b ->{f} c)
                                 -> Map.Nonempty k a
                                 -> Map.Nonempty k b
                                 ->{e, f} Map.Nonempty k c
data.Map.Nonempty.alter : (Optional v ->{e} Optional v)
                          -> k
                          -> Map.Nonempty k v
                          ->{e} Map k v
data.Map.Nonempty.Bin : Nat -> k -> v -> Map k v -> Map k v -> Map.Nonempty k v
data.Map.Nonempty.breakOffMax : Map.Nonempty k v -> ((k, v), Map k v)
data.Map.Nonempty.breakOffMin : Map.Nonempty k v -> ((k, v), Map k v)
data.Map.Nonempty.contains : k -> Map.Nonempty k v -> Boolean
data.Map.Nonempty.delete : k -> Map.Nonempty k v -> Map k v
data.Map.Nonempty.filter : (v ->{g} Boolean) -> Map.Nonempty k v ->{g} Map k v
data.Map.Nonempty.filterAlignWithKey : (k ->{e} OneOrBoth a b ->{f} Optional c)
                                       -> Map.Nonempty k a
                                       -> Map.Nonempty k b
                                       ->{e, f} Map k c
data.Map.Nonempty.filterKeys : (k ->{g} Boolean) -> Map.Nonempty k v ->{g} Map k v
data.Map.Nonempty.filterWithKey : (k ->{e} a ->{f} Boolean)
                                  -> Map.Nonempty k a
                                  ->{e, f} Map k a
data.Map.Nonempty.foldLeft : (a ->{e} b ->{e} a) -> a -> Map.Nonempty k b ->{e} a
data.Map.Nonempty.foldLeftWithKey : (a ->{e} k ->{e} b ->{e} a)
                                    -> a
                                    -> Map.Nonempty k b
                                    ->{e} a
data.Map.Nonempty.foldMap : (a ->{e} a ->{e} a)
                            -> (k ->{e} v ->{e} a)
                            -> Map.Nonempty k v
                            ->{e} a
data.Map.Nonempty.foldRight : (a ->{e} b ->{e} b) -> b -> Map.Nonempty k a ->{e} b
data.Map.Nonempty.foldRightWithKey : (k ->{e} a ->{e} b ->{e} b)
                                     -> b
                                     -> Map.Nonempty k a
                                     ->{e} b
data.Map.Nonempty.foreach : Map.Nonempty k v -> (k ->{e} v ->{e} ()) ->{e} ()
data.Map.Nonempty.fromList : List.Nonempty (k, v) -> Map.Nonempty k v
data.Map.Nonempty.fromListWith : (v ->{e} v ->{e} v)
                                 -> List.Nonempty (k, v)
                                 ->{e} Map.Nonempty k v
data.Map.Nonempty.fromListWithKey : (k ->{e} v ->{e} v ->{e} v)
                                    -> List.Nonempty (k, v)
                                    ->{e} Map.Nonempty k v
data.Map.Nonempty.get : k -> Map.Nonempty k v -> Optional v
data.Map.Nonempty.getMax : Map.Nonempty k v -> Optional (k, v)
data.Map.Nonempty.getMin : Map.Nonempty k v -> Optional (k, v)
data.Map.Nonempty.getOrAbort : k -> Map.Nonempty k v ->{Abort} v
data.Map.Nonempty.getOrElse : v -> k -> Map.Nonempty k v -> v
data.Map.Nonempty.getOrThrow : e -> k -> Map.Nonempty k v ->{Throw e} v
data.Map.Nonempty.insert : k -> v -> Map.Nonempty k v -> Map.Nonempty k v
data.Map.Nonempty.internal.balanceL : k -> v -> Map k v -> Map k v -> Map.Nonempty k v
data.Map.Nonempty.internal.balanceR : k -> v -> Map k v -> Map k v -> Map.Nonempty k v
data.Map.Nonempty.internal.link : k -> v -> Map k v -> Map k v -> Map.Nonempty k v
data.Map.Nonempty.internal.putMax : k -> v -> Map k v -> Map.Nonempty k v
data.Map.Nonempty.internal.putMin : k -> v -> Map k v -> Map.Nonempty k v
data.Map.Nonempty.internal.putWithKeyR : (k ->{e} v ->{e} v ->{e} v)
                                         -> k
                                         -> v
                                         -> Map.Nonempty k v
                                         ->{e} Map.Nonempty k v
data.Map.Nonempty.intersect : Map.Nonempty k a -> Map.Nonempty k b -> Map k a
data.Map.Nonempty.intersectWith : (a ->{e} b ->{e} c)
                                  -> Map.Nonempty k a
                                  -> Map.Nonempty k b
                                  ->{e} Map k c
data.Map.Nonempty.intersectWithKey : (k ->{e} a ->{e} b ->{e} c)
                                     -> Map.Nonempty k a
                                     -> Map.Nonempty k b
                                     ->{e} Map k c
data.Map.Nonempty.keys : Map.Nonempty k a -> List.Nonempty k
data.Map.Nonempty.map : (a ->{e} b) -> Map.Nonempty k a ->{e} Map.Nonempty k b
data.Map.Nonempty.mapKeys : (k1 ->{g} k2) -> Map.Nonempty k1 v ->{g} Map.Nonempty k2 v
data.Map.Nonempty.mapKeysWith : (v ->{g} v ->{g} v)
                                -> (k1 ->{g} k2)
                                -> Map.Nonempty k1 v
                                ->{g} Map.Nonempty k2 v
data.Map.Nonempty.mapWithKey : (k ->{e} a ->{e} b)
                               -> Map.Nonempty k a
                               ->{e} Map.Nonempty k b
data.Map.Nonempty.mergeWith : (OneOrBoth a b ->{g} Optional c)
                              -> Map.Nonempty k a
                              -> Map.Nonempty k b
                              ->{g} Map k c
data.Map.Nonempty.mergeWithKey : (k ->{e} OneOrBoth a b ->{f} Optional c)
                                 -> Map.Nonempty k a
                                 -> Map.Nonempty k b
                                 ->{e, f} Map k c
data.Map.Nonempty.nth : Nat -> Map.Nonempty k v -> Optional (k, v)
data.Map.Nonempty.putGetWithKey : (k ->{e} v ->{e} v ->{e} v)
                                  -> k
                                  -> v
                                  -> Map.Nonempty k v
                                  ->{e} (Optional v, Map.Nonempty k v)
data.Map.Nonempty.putWith : (v ->{e} v ->{e} v)
                            -> k
                            -> v
                            -> Map.Nonempty k v
                            ->{e} Map.Nonempty k v
data.Map.Nonempty.putWithKey : (k ->{e} v ->{e} v ->{e} v)
                               -> k
                               -> v
                               -> Map.Nonempty k v
                               ->{e} Map.Nonempty k v
data.Map.Nonempty.randomChoice : Map.Nonempty k v ->{Random} (k, v)
data.Map.Nonempty.randomKey : Map.Nonempty k v ->{Random} k
data.Map.Nonempty.randomValue : Map.Nonempty k v ->{Random} v
data.Map.Nonempty.singleton : k -> v -> Map.Nonempty k v
data.Map.Nonempty.size : Map.Nonempty k v -> Nat
data.Map.Nonempty.toList : Map.Nonempty k v -> [(k, v)]
data.Map.Nonempty.toMap : Map.Nonempty k v -> Map k v
data.Map.Nonempty.toNonemptyList : Map.Nonempty k v -> List.Nonempty (k, v)
data.Map.Nonempty.toNonemptyMap : List.Nonempty (k, v) -> Map.Nonempty k v
data.Map.Nonempty.union : Map.Nonempty k v -> Map.Nonempty k v -> Map.Nonempty k v
data.Map.Nonempty.unions : List.Nonempty (Map.Nonempty k v) -> Map.Nonempty k v
data.Map.Nonempty.unionWith : (v ->{e} v ->{e} v)
                              -> Map.Nonempty k v
                              -> Map.Nonempty k v
                              ->{e} Map.Nonempty k v
data.Map.Nonempty.unionWithKey : (k ->{e} v ->{e} v ->{e} v)
                                 -> Map.Nonempty k v
                                 -> Map.Nonempty k v
                                 ->{e} Map.Nonempty k v
data.Map.Nonempty.update : (v ->{e} Optional v) -> k -> Map.Nonempty k v ->{e} Map k v
data.Map.Nonempty.upsert : (Optional v ->{e} v)
                           -> k
                           -> Map.Nonempty k v
                           ->{e} Map.Nonempty k v
data.Map.Nonempty.values : Map.Nonempty k v -> List.Nonempty v
data.Map.nth : Nat -> Map k v -> Optional (k, v)
data.Map.put : k -> v -> Map k v -> Map k v
data.Map.putGetWithKey : (k ->{e} v ->{e} v ->{e} v)
                         -> k
                         -> v
                         -> Map k v
                         ->{e} (Optional v, Map k v)
data.Map.putWith : (v ->{e} v ->{e} v) -> k -> v -> Map k v ->{e} Map k v
data.Map.putWithKey : (k ->{e} v ->{e} v ->{e} v) -> k -> v -> Map k v ->{e} Map k v
data.Map.randomChoice : Map k v ->{Exception, Random} (k, v)
data.Map.randomKey : Map k v ->{Exception, Random} k
data.Map.randomValue : Map k v ->{Exception, Random} v
data.Map.singleton : k -> v -> Map k v
data.Map.size : Map k v -> Nat
data.Map.split : k -> Map k a -> (Map k a, Map k a)
data.Map.takeLargest : Nat -> Map k v -> Map k v
data.Map.takeSmallest : Nat -> Map k v -> Map k v
data.Map.toList : Map k v -> [(k, v)]
data.Map.toStream : Map k b -> '{Stream (k, b)} ()
data.Map.union : Map k v -> Map k v -> Map k v
data.Map.unions : [Map k v] -> Map k v
data.Map.unionWith : (v ->{e} v ->{e} v) -> Map k v -> Map k v ->{e} Map k v
data.Map.unionWithKey : (k ->{e} v ->{e} v ->{e} v) -> Map k v -> Map k v ->{e} Map k v
data.Map.update : (v ->{e} Optional v) -> k -> Map k v ->{e} Map k v
data.Map.updateWithKey : (k ->{e} v ->{e} Optional v) -> k -> Map k v ->{e} Map k v
data.Map.upsert : (Optional a ->{g} a) -> k -> Map k a ->{g} Map k a
data.Map.values : Map k v -> [v]
data.Multimap.insert : k -> v -> Map k [v] -> Map k [v]
data.Multimap.lookup : k -> Map k [elem] -> [elem]
type data.NatBag
data.NatBag.add : Nat -> NatBag -> NatBag
data.NatBag.add.nonempty : Nat -> NatBag -> NatBag.Nonempty
data.NatBag.addAll : NatBag -> NatBag -> NatBag
data.NatBag.addMany : NatBag -> [Nat] -> NatBag
data.NatBag.addN : Nat -> Nat -> NatBag -> NatBag
data.NatBag.all : (Nat ->{g} Boolean) ->{g} NatBag ->{g} Boolean
data.NatBag.any : (Nat ->{g} Boolean) ->{g} NatBag ->{g} Boolean
data.NatBag.contains : Nat -> NatBag -> Boolean
data.NatBag.convolve : (Nat ->{e} Nat ->{e} Nat) -> NatBag -> NatBag ->{e} NatBag
data.NatBag.count : Nat -> NatBag -> Nat
data.NatBag.counts : NatBag -> NatMap Nat
data.NatBag.difference : NatBag -> NatBag -> NatBag
data.NatBag.empty : NatBag
data.NatBag.equals : NatBag -> NatBag -> Boolean
data.NatBag.filter : (Nat ->{e} Boolean) -> NatBag ->{e} NatBag
data.NatBag.filterMap : (Nat ->{e} Optional Nat) -> NatBag ->{e} NatBag
data.NatBag.flatMap : (Nat ->{e} NatBag) -> NatBag ->{e} NatBag
data.NatBag.foldLeft : (a ->{e} Nat ->{e} a) -> a -> NatBag ->{e} a
data.NatBag.foldRight : (Nat ->{e} a ->{e} a) -> a -> NatBag ->{e} a
data.NatBag.from : NatBag -> NatBag -> Boolean
data.NatBag.fromBag : Bag Nat -> NatBag
data.NatBag.fromList : [Nat] -> NatBag
data.NatBag.fromNatSet : NatSet -> NatBag
data.NatBag.fromOccurrenceList : [(Nat, Nat)] -> NatBag
data.NatBag.getMax : NatBag ->{Abort} Nat
data.NatBag.getMin : NatBag ->{Abort} Nat
data.NatBag.intersect : NatBag -> NatBag -> NatBag
data.NatBag.isEmpty : NatBag -> Boolean
data.NatBag.map : (Nat ->{e} Nat) -> NatBag ->{e} NatBag
data.NatBag.NatBag : NatMap Nat -> NatBag
type data.NatBag.Nonempty
data.NatBag.Nonempty.add : Nat -> NatBag.Nonempty -> NatBag.Nonempty
data.NatBag.Nonempty.addAll : NatBag.Nonempty -> NatBag.Nonempty -> NatBag.Nonempty
data.NatBag.Nonempty.addMany : NatBag.Nonempty -> [Nat] -> NatBag.Nonempty
data.NatBag.Nonempty.addN : Nat -> Nat -> NatBag.Nonempty -> NatBag.Nonempty
data.NatBag.Nonempty.count : Nat -> NatBag.Nonempty -> Nat
data.NatBag.Nonempty.counts : NatBag.Nonempty -> NatMap.Nonempty Nat
data.NatBag.Nonempty.difference : NatBag.Nonempty -> NatBag.Nonempty -> NatBag
data.NatBag.Nonempty.equals : NatBag.Nonempty -> NatBag.Nonempty -> Boolean
data.NatBag.Nonempty.fromList : List.Nonempty Nat -> NatBag.Nonempty
data.NatBag.Nonempty.fromNatSet : NatSet.Nonempty -> NatBag.Nonempty
data.NatBag.Nonempty.fromOccurrenceList : List.Nonempty (Nat, Nat) -> NatBag.Nonempty
data.NatBag.Nonempty.getMax : NatBag.Nonempty -> Nat
data.NatBag.Nonempty.getMin : NatBag.Nonempty -> Nat
data.NatBag.Nonempty.intersect : NatBag.Nonempty -> NatBag.Nonempty -> NatBag
data.NatBag.Nonempty.NatBag.Nonempty : NatMap.Nonempty Nat -> NatBag.Nonempty
data.NatBag.Nonempty.nth : Nat -> NatBag.Nonempty -> Optional Nat
data.NatBag.Nonempty.occurrenceList : NatBag.Nonempty -> List.Nonempty (Nat, Nat)
data.NatBag.Nonempty.randomChoice : NatBag.Nonempty ->{Random} Nat
data.NatBag.Nonempty.remove : Nat -> NatBag.Nonempty -> NatBag
data.NatBag.Nonempty.removeAll : Nat -> NatBag.Nonempty -> NatBag
data.NatBag.Nonempty.removeMax : NatBag.Nonempty -> NatBag
data.NatBag.Nonempty.removeMin : NatBag.Nonempty -> NatBag
data.NatBag.Nonempty.removeN : Nat -> Nat -> NatBag.Nonempty -> NatBag
data.NatBag.Nonempty.size : NatBag.Nonempty -> Nat
data.NatBag.Nonempty.subbag : NatBag.Nonempty -> NatBag.Nonempty -> Boolean
data.NatBag.Nonempty.superbag : NatBag.Nonempty -> NatBag.Nonempty -> Boolean
data.NatBag.Nonempty.toBag : NatBag.Nonempty -> Bag Nat
data.NatBag.Nonempty.toList : NatBag.Nonempty -> List.Nonempty Nat
data.NatBag.Nonempty.toNatBag : NatBag.Nonempty -> NatBag
data.NatBag.Nonempty.toNatSet : NatBag.Nonempty -> NatSet.Nonempty
data.NatBag.Nonempty.union : NatBag.Nonempty -> NatBag.Nonempty -> NatBag.Nonempty
data.NatBag.nth : Nat -> NatBag -> Optional Nat
data.NatBag.occurrenceList : NatBag -> [(Nat, Nat)]
data.NatBag.partition : (Nat ->{e} Boolean) -> NatBag ->{e} (NatBag, NatBag)
data.NatBag.randomChoice : NatBag ->{Random} Nat
data.NatBag.remove : Nat -> NatBag -> NatBag
data.NatBag.removeAll : Nat -> NatBag -> NatBag
data.NatBag.removeN : Nat -> Nat -> NatBag -> NatBag
data.NatBag.scale : Nat -> NatBag -> NatBag
data.NatBag.singleton : Nat -> NatBag.Nonempty
data.NatBag.size : NatBag -> Nat
data.NatBag.subbag : NatBag -> NatBag -> Boolean
data.NatBag.superbag : NatBag -> NatBag -> Boolean
data.NatBag.toBag : NatBag -> Bag Nat
data.NatBag.toList : NatBag -> [Nat]
data.NatBag.toNatSet : NatBag -> NatSet
data.NatBag.union : NatBag -> NatBag -> NatBag
type data.NatMap a
data.NatMap.adjust : (a ->{g} a) -> Nat -> NatMap a ->{g} NatMap a
data.NatMap.adjustWithKey : (Nat ->{g} a ->{g} a) -> Nat -> NatMap a ->{g} NatMap a
data.NatMap.align : NatMap a -> NatMap b -> NatMap (OneOrBoth a b)
data.NatMap.alignWith : (OneOrBoth a b ->{g} c) -> NatMap a -> NatMap b ->{g} NatMap c
data.NatMap.alignWithKey : (Nat ->{e} OneOrBoth a b ->{f} c)
                           -> NatMap a
                           -> NatMap b
                           ->{e, f} NatMap c
data.NatMap.alter : (Optional a ->{g} Optional a) -> Nat -> NatMap a ->{g} NatMap a
data.NatMap.alterWithKey : (Nat ->{g} Optional a ->{g} Optional a)
                           -> Nat
                           -> NatMap a
                           ->{g} NatMap a
data.NatMap.breakOffMax : NatMap a ->{Abort} ((Nat, a), NatMap a)
data.NatMap.breakOffMin : NatMap a ->{Abort} ((Nat, a), NatMap a)
data.NatMap.compareBy : (a ->{g} a ->{g} Ordering) -> NatMap a -> NatMap a ->{g} Ordering
data.NatMap.contains : Nat -> NatMap a -> Boolean
data.NatMap.delete : Nat -> NatMap a -> NatMap a
data.NatMap.deleteMax : NatMap a ->{Abort} NatMap a
data.NatMap.deleteMin : NatMap a ->{Abort} NatMap a
data.NatMap.difference : NatMap a -> NatMap b -> NatMap a
data.NatMap.differenceWith : (a ->{g} b ->{g} Optional a)
                             -> NatMap a
                             -> NatMap b
                             ->{g} NatMap a
data.NatMap.differenceWithKey : (Nat ->{g} a ->{g} b ->{g} Optional a)
                                -> NatMap a
                                -> NatMap b
                                ->{g} NatMap a
data.NatMap.empty : NatMap a
data.NatMap.equalBy : (a ->{g} a ->{g} Boolean) -> NatMap a -> NatMap a ->{g} Boolean
data.NatMap.filter : (a ->{g} Boolean) -> NatMap a ->{g} NatMap a
data.NatMap.filterWithKey : (Nat ->{g} a ->{g} Boolean) -> NatMap a ->{g} NatMap a
data.NatMap.foldLeft : (a ->{g} b ->{g} b) -> b -> NatMap a ->{g} b
data.NatMap.foldLeftWithKey : (Nat ->{g} a ->{g} b ->{g} b) -> b -> NatMap a ->{g} b
data.NatMap.foldRight : (a ->{g} b ->{g} b) -> b -> NatMap a ->{g} b
data.NatMap.foldRightWithKey : (Nat ->{g} a ->{g} b ->{g} b) -> b -> NatMap a ->{g} b
data.NatMap.fromList : [(Nat, a)] -> NatMap a
data.NatMap.fromListWith : (a ->{g} a ->{g} a) -> [(Nat, a)] ->{g} NatMap a
data.NatMap.fromListWithKey : (Nat ->{g} a ->{g} a ->{g} a) -> [(Nat, a)] ->{g} NatMap a
data.NatMap.fromMap : Map Nat a -> NatMap a
data.NatMap.get : Nat -> NatMap a -> Optional a
data.NatMap.getAbove : Nat -> NatMap v -> Optional (Nat, v)
data.NatMap.getAtLeast : Nat -> NatMap v -> Optional (Nat, v)
data.NatMap.getAtMost : Nat -> NatMap v -> Optional (Nat, v)
data.NatMap.getBelow : Nat -> NatMap v -> Optional (Nat, v)
data.NatMap.getMax : NatMap a ->{Abort} a
data.NatMap.getMin : NatMap a ->{Abort} a
data.NatMap.getOrAbort : Nat -> NatMap a ->{Abort} a
data.NatMap.getOrElse : Nat -> a -> NatMap a -> a
data.NatMap.insert : Nat -> a -> NatMap a -> NatMap a
data.NatMap.insert.nonempty : Nat -> a -> NatMap a -> NatMap.Nonempty a
data.NatMap.insert.nonempty.nonempty : Nat -> a -> NatMap a -> NatMap.Nonempty a
data.NatMap.insertGetWithKey : (Nat ->{g} a ->{g} a ->{g} a)
                               -> Nat
                               -> a
                               -> NatMap a
                               ->{g} (Optional a, NatMap.Nonempty a)
data.NatMap.insertWith : (a ->{g} a ->{g} a)
                         -> Nat
                         -> a
                         -> NatMap a
                         ->{g} NatMap.Nonempty a
data.NatMap.insertWithKey : (Nat ->{g} a ->{g} a ->{g} a)
                            -> Nat
                            -> a
                            -> NatMap a
                            ->{g} NatMap.Nonempty a
data.NatMap.internal.bim : Nat
                           -> Nat
                           -> NatMap.Nonempty a
                           -> NatMap.Nonempty a
                           -> NatMap.Nonempty a
data.NatMap.internal.bin : Nat -> Nat -> NatMap a -> NatMap a -> NatMap a
data.NatMap.internal.branchMask : Nat -> Nat -> Nat
data.NatMap.internal.highBitMask : Nat -> Nat
data.NatMap.internal.join : Nat
                            -> NatMap.Nonempty a
                            -> Nat
                            -> NatMap.Nonempty a
                            -> NatMap.Nonempty a
data.NatMap.internal.mask : Nat -> Nat -> Nat
data.NatMap.internal.nomatch : Nat -> Nat -> Nat -> Boolean
data.NatMap.internal.shorter : Nat -> Nat -> Boolean
data.NatMap.internal.zero : Nat -> Nat -> Boolean
data.NatMap.intersect : NatMap a -> NatMap b -> NatMap a
data.NatMap.intersectWith : (a ->{g} b ->{g} a) -> NatMap a -> NatMap b ->{g} NatMap a
data.NatMap.intersectWithKey : (Nat ->{g} a ->{g} b ->{g} a)
                               -> NatMap a
                               -> NatMap b
                               ->{g} NatMap a
data.NatMap.isEmpty : NatMap a -> Boolean
data.NatMap.isProperSubmapOf : NatMap a -> NatMap a -> Boolean
data.NatMap.isProperSubmapOfBy : (a ->{g} a ->{g} Boolean)
                                 -> NatMap a
                                 -> NatMap a
                                 ->{g} Boolean
data.NatMap.isSubmapOf : NatMap a -> NatMap a -> Boolean
data.NatMap.isSubmapOfBy : (a ->{g} a ->{g} Boolean) -> NatMap a -> NatMap a ->{g} Boolean
data.NatMap.keys : NatMap a -> [Nat]
data.NatMap.keySet : NatMap a -> NatSet
data.NatMap.lookup : Nat -> NatMap a -> Optional a
data.NatMap.map : (a ->{g} b) -> NatMap a ->{g} NatMap b
data.NatMap.mapEither : (a ->{g} Either b c) -> NatMap a ->{g} (NatMap b, NatMap c)
data.NatMap.mapEitherWithKey : (Nat ->{g} a ->{g} Either b c)
                               -> NatMap a
                               ->{g} (NatMap b, NatMap c)
data.NatMap.mapOptional : (a ->{g} Optional b) -> NatMap a ->{g} NatMap b
data.NatMap.mapOptionalWithKey : (Nat ->{g} a ->{g} Optional b) -> NatMap a ->{g} NatMap b
data.NatMap.mapWithKey : (Nat ->{g} a ->{g} b) -> NatMap a ->{g} NatMap b
data.NatMap.maxKey : NatMap a ->{Abort} Nat
data.NatMap.maxView : NatMap a ->{Abort} (a, NatMap a)
data.NatMap.minKey : NatMap a ->{Abort} Nat
data.NatMap.minView : NatMap a ->{Abort} (a, NatMap a)
data.NatMap.NatMap : Optional (NatMap.Nonempty a) -> NatMap a
type data.NatMap.Nonempty a
data.NatMap.Nonempty.adjust : (a ->{g} a)
                              -> Nat
                              -> NatMap.Nonempty a
                              ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.adjustWithKey : (Nat ->{g} a ->{g} a)
                                     -> Nat
                                     -> NatMap.Nonempty a
                                     ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.align : NatMap.Nonempty a
                             -> NatMap.Nonempty b
                             -> NatMap.Nonempty (OneOrBoth a b)
data.NatMap.Nonempty.alignWith : (OneOrBoth a b ->{g} c)
                                 -> NatMap.Nonempty a
                                 -> NatMap.Nonempty b
                                 ->{g} NatMap.Nonempty c
data.NatMap.Nonempty.alignWithKey : (Nat ->{e} OneOrBoth a b ->{f} c)
                                    -> NatMap.Nonempty a
                                    -> NatMap.Nonempty b
                                    ->{e, f} NatMap.Nonempty c
data.NatMap.Nonempty.alter : (Optional a ->{g} Optional a)
                             -> Nat
                             -> NatMap.Nonempty a
                             ->{g} NatMap a
data.NatMap.Nonempty.alterWithKey : (Nat ->{g} Optional a ->{g} Optional a)
                                    -> Nat
                                    -> NatMap.Nonempty a
                                    ->{g} NatMap a
data.NatMap.Nonempty.breakOffMax : NatMap.Nonempty a -> ((Nat, a), NatMap a)
data.NatMap.Nonempty.breakOffMin : NatMap.Nonempty a -> ((Nat, a), NatMap a)
data.NatMap.Nonempty.compareBy : (a ->{g} a ->{g} Ordering)
                                 -> NatMap.Nonempty a
                                 -> NatMap.Nonempty a
                                 ->{g} Ordering
data.NatMap.Nonempty.contains : Nat -> NatMap.Nonempty a -> Boolean
data.NatMap.Nonempty.delete : Nat -> NatMap.Nonempty a -> NatMap a
data.NatMap.Nonempty.deleteMax : NatMap.Nonempty a -> NatMap a
data.NatMap.Nonempty.deleteMin : NatMap.Nonempty a -> NatMap a
data.NatMap.Nonempty.difference : NatMap.Nonempty a -> NatMap.Nonempty b -> NatMap a
data.NatMap.Nonempty.differenceWith : (a ->{g} b ->{g} Optional a)
                                      -> NatMap.Nonempty a
                                      -> NatMap.Nonempty b
                                      ->{g} NatMap a
data.NatMap.Nonempty.differenceWithKey : (Nat ->{g} a ->{g} b ->{g} Optional a)
                                         -> NatMap.Nonempty a
                                         -> NatMap.Nonempty b
                                         ->{g} NatMap a
data.NatMap.Nonempty.equalBy : (a ->{g} a ->{g} Boolean)
                               -> NatMap.Nonempty a
                               -> NatMap.Nonempty a
                               ->{g} Boolean
data.NatMap.Nonempty.filter : (a ->{g} Boolean) -> NatMap.Nonempty a ->{g} NatMap a
data.NatMap.Nonempty.filterWithKey : (Nat ->{g} a ->{g} Boolean)
                                     -> NatMap.Nonempty a
                                     ->{g} NatMap a
data.NatMap.Nonempty.foldLeft : (a ->{g} b ->{g} b) -> b -> NatMap.Nonempty a ->{g} b
data.NatMap.Nonempty.foldLeftWithKey : (Nat ->{g} a ->{g} b ->{g} b)
                                       -> b
                                       -> NatMap.Nonempty a
                                       ->{g} b
data.NatMap.Nonempty.foldMap : (b ->{g} b ->{g} b)
                               -> (a ->{g} b)
                               -> NatMap.Nonempty a
                               ->{g} b
data.NatMap.Nonempty.foldMapWithKey : (b ->{g} b ->{g} b)
                                      -> (Nat ->{g} a ->{g} b)
                                      -> NatMap.Nonempty a
                                      ->{g} b
data.NatMap.Nonempty.foldRight : (a ->{g} b ->{g} b) -> b -> NatMap.Nonempty a ->{g} b
data.NatMap.Nonempty.foldRightWithKey : (Nat ->{g} a ->{g} b ->{g} b)
                                        -> b
                                        -> NatMap.Nonempty a
                                        ->{g} b
data.NatMap.Nonempty.fromList : List.Nonempty (Nat, a) -> NatMap.Nonempty a
data.NatMap.Nonempty.fromListWith : (a ->{g} a ->{g} a)
                                    -> List.Nonempty (Nat, a)
                                    ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.fromListWithKey : (Nat ->{g} a ->{g} a ->{g} a)
                                       -> List.Nonempty (Nat, a)
                                       ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.get : Nat -> NatMap.Nonempty a -> Optional a
data.NatMap.Nonempty.getAbove : Nat -> NatMap.Nonempty v -> Optional (Nat, v)
data.NatMap.Nonempty.getAtLeast : Nat -> NatMap.Nonempty v -> Optional (Nat, v)
data.NatMap.Nonempty.getAtMost : Nat -> NatMap.Nonempty v -> Optional (Nat, v)
data.NatMap.Nonempty.getBelow : Nat -> NatMap.Nonempty v -> Optional (Nat, v)
data.NatMap.Nonempty.getMax : NatMap.Nonempty a -> a
data.NatMap.Nonempty.getMin : NatMap.Nonempty a -> a
data.NatMap.Nonempty.getOrAbort : Nat -> NatMap.Nonempty a ->{Abort} a
data.NatMap.Nonempty.getOrElse : Nat -> a -> NatMap.Nonempty a -> a
data.NatMap.Nonempty.insert : Nat -> a -> NatMap.Nonempty a -> NatMap.Nonempty a
data.NatMap.Nonempty.insertGetWithKey : (Nat ->{g} a ->{g} a ->{g} a)
                                        -> Nat
                                        -> a
                                        -> NatMap.Nonempty a
                                        ->{g} (Optional a, NatMap.Nonempty a)
data.NatMap.Nonempty.insertWith : (a ->{g} a ->{g} a)
                                  -> Nat
                                  -> a
                                  -> NatMap.Nonempty a
                                  ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.insertWithKey : (Nat ->{g} a ->{g} a ->{g} a)
                                     -> Nat
                                     -> a
                                     -> NatMap.Nonempty a
                                     ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.intersect : NatMap.Nonempty a -> NatMap.Nonempty b -> NatMap a
data.NatMap.Nonempty.intersectWith : (a ->{g} b ->{g} a)
                                     -> NatMap.Nonempty a
                                     -> NatMap.Nonempty b
                                     ->{g} NatMap a
data.NatMap.Nonempty.intersectWithKey : (Nat ->{g} a ->{g} b ->{g} a)
                                        -> NatMap.Nonempty a
                                        -> NatMap.Nonempty b
                                        ->{g} NatMap a
data.NatMap.Nonempty.isProperSubmapOf : NatMap.Nonempty a -> NatMap.Nonempty a -> Boolean
data.NatMap.Nonempty.isProperSubmapOfBy : (a ->{g} a ->{g} Boolean)
                                          -> NatMap.Nonempty a
                                          -> NatMap.Nonempty a
                                          ->{g} Boolean
data.NatMap.Nonempty.isSubmapOf : NatMap.Nonempty a -> NatMap.Nonempty a -> Boolean
data.NatMap.Nonempty.isSubmapOfBy : (a ->{g} a ->{g} Boolean)
                                    -> NatMap.Nonempty a
                                    -> NatMap.Nonempty a
                                    ->{g} Boolean
data.NatMap.Nonempty.keys : NatMap.Nonempty a -> List.Nonempty Nat
data.NatMap.Nonempty.keySet : NatMap.Nonempty a -> NatSet.Nonempty
data.NatMap.Nonempty.map : (a ->{g} b) -> NatMap.Nonempty a ->{g} NatMap.Nonempty b
data.NatMap.Nonempty.mapEither : (a ->{g} Either b c)
                                 -> NatMap.Nonempty a
                                 ->{g} (NatMap b, NatMap c)
data.NatMap.Nonempty.mapEitherWithKey : (Nat ->{g} a ->{g} Either b c)
                                        -> NatMap.Nonempty a
                                        ->{g} (NatMap b, NatMap c)
data.NatMap.Nonempty.mapOptional : (a ->{g} Optional b) -> NatMap.Nonempty a ->{g} NatMap b
data.NatMap.Nonempty.mapOptionalWithKey : (Nat ->{g} a ->{g} Optional b)
                                          -> NatMap.Nonempty a
                                          ->{g} NatMap b
data.NatMap.Nonempty.mapWithKey : (Nat ->{g} a ->{g} b)
                                  -> NatMap.Nonempty a
                                  ->{g} NatMap.Nonempty b
data.NatMap.Nonempty.maxKey : NatMap.Nonempty a -> Nat
data.NatMap.Nonempty.maxView : NatMap.Nonempty a -> (a, NatMap a)
data.NatMap.Nonempty.minKey : NatMap.Nonempty a -> Nat
data.NatMap.Nonempty.minView : NatMap.Nonempty a -> (a, NatMap a)
data.NatMap.Nonempty.NatMap.Nonempty.Bin : Nat
                                           -> Nat
                                           -> Nat
                                           -> NatMap.Nonempty a
                                           -> NatMap.Nonempty a
                                           -> NatMap.Nonempty a
data.NatMap.Nonempty.NatMap.Nonempty.Tip : Nat -> a -> NatMap.Nonempty a
data.NatMap.Nonempty.nth : Nat -> NatMap.Nonempty v -> Optional (Nat, v)
data.NatMap.Nonempty.partition : (a ->{g} Boolean)
                                 -> NatMap.Nonempty a
                                 ->{g} (NatMap a, NatMap a)
data.NatMap.Nonempty.partitionWithKey : (Nat ->{g} a ->{g} Boolean)
                                        -> NatMap.Nonempty a
                                        ->{g} (NatMap a, NatMap a)
data.NatMap.Nonempty.randomChoice : NatMap.Nonempty v ->{Random} (Nat, v)
data.NatMap.Nonempty.randomKey : NatMap.Nonempty v ->{Random} Nat
data.NatMap.Nonempty.randomValue : NatMap.Nonempty v ->{Random} v
data.NatMap.Nonempty.restrict : Nat -> Nat -> NatMap.Nonempty v -> NatMap v
data.NatMap.Nonempty.restrictAbove : Nat -> NatMap.Nonempty v -> NatMap v
data.NatMap.Nonempty.restrictBelow : Nat -> NatMap.Nonempty v -> NatMap v
data.NatMap.Nonempty.singleton : Nat -> a -> NatMap.Nonempty a
data.NatMap.Nonempty.size : NatMap.Nonempty a -> Nat
data.NatMap.Nonempty.split : Nat -> NatMap.Nonempty a -> (NatMap a, Optional a, NatMap a)
data.NatMap.Nonempty.submapCompareBy : (a ->{g} a ->{g} Boolean)
                                       -> NatMap.Nonempty a
                                       -> NatMap.Nonempty a
                                       ->{g} Optional Ordering
data.NatMap.Nonempty.test.gen : '{Gen} t -> '{Gen} NatMap.Nonempty t
data.NatMap.Nonempty.toList : NatMap.Nonempty a -> List.Nonempty (Nat, a)
data.NatMap.Nonempty.toMap : NatMap.Nonempty a -> Map.Nonempty Nat a
data.NatMap.Nonempty.toNatMap : NatMap.Nonempty a -> NatMap a
data.NatMap.Nonempty.union : NatMap.Nonempty a -> NatMap.Nonempty a -> NatMap.Nonempty a
data.NatMap.Nonempty.unions : List.Nonempty (NatMap.Nonempty a) -> NatMap.Nonempty a
data.NatMap.Nonempty.unionsWith : (a ->{g} a ->{g} a)
                                  -> List.Nonempty (NatMap.Nonempty a)
                                  ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.unionWith : (a ->{g} a ->{g} a)
                                 -> NatMap.Nonempty a
                                 -> NatMap.Nonempty a
                                 ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.unionWithKey : (Nat ->{g} a ->{g} a ->{g} a)
                                    -> NatMap.Nonempty a
                                    -> NatMap.Nonempty a
                                    ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.update : (a ->{g} Optional a)
                              -> Nat
                              -> NatMap.Nonempty a
                              ->{g} NatMap a
data.NatMap.Nonempty.updateGetWithKey : (Nat ->{g} a ->{g} Optional a)
                                        -> Nat
                                        -> NatMap.Nonempty a
                                        ->{g} (Optional a, NatMap a)
data.NatMap.Nonempty.updateMax : (a ->{g} a) -> NatMap.Nonempty a ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.updateMaxWithKey : (Nat ->{g} a ->{g} a)
                                        -> NatMap.Nonempty a
                                        ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.updateMin : (a ->{g} a) -> NatMap.Nonempty a ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.updateMinWithKey : (Nat ->{g} a ->{g} a)
                                        -> NatMap.Nonempty a
                                        ->{g} NatMap.Nonempty a
data.NatMap.Nonempty.updateWithKey : (Nat ->{g} a ->{g} Optional a)
                                     -> Nat
                                     -> NatMap.Nonempty a
                                     ->{g} NatMap a
data.NatMap.Nonempty.values : NatMap.Nonempty a -> List.Nonempty a
data.NatMap.nth : Nat -> NatMap v -> Optional (Nat, v)
data.NatMap.partition : (a ->{g} Boolean) -> NatMap a ->{g} (NatMap a, NatMap a)
data.NatMap.partitionWithKey : (Nat ->{g} a ->{g} Boolean)
                               -> NatMap a
                               ->{g} (NatMap a, NatMap a)
data.NatMap.randomChoice : NatMap v ->{Exception, Random} (Nat, v)
data.NatMap.randomKey : NatMap v ->{Exception, Random} Nat
data.NatMap.randomValue : NatMap v ->{Exception, Random} v
data.NatMap.restrict : Nat -> Nat -> NatMap v -> NatMap v
data.NatMap.restrictAbove : Nat -> NatMap v -> NatMap v
data.NatMap.restrictBelow : Nat -> NatMap v -> NatMap v
data.NatMap.singleton : Nat -> a -> NatMap.Nonempty a
data.NatMap.size : NatMap a -> Nat
data.NatMap.split : Nat -> NatMap a -> (NatMap a, Optional a, NatMap a)
data.NatMap.submapCompareBy : (a ->{g} a ->{g} Boolean)
                              -> NatMap a
                              -> NatMap a
                              ->{g} Optional Ordering
data.NatMap.test.gen : '{Gen} t -> '{Gen} NatMap t
data.NatMap.toList : NatMap a -> [(Nat, a)]
data.NatMap.toMap : NatMap a -> Map Nat a
data.NatMap.toNonemptyList : NatMap.Nonempty a -> List.Nonempty (Nat, a)
data.NatMap.union : NatMap a -> NatMap a -> NatMap a
data.NatMap.unions : [NatMap a] -> NatMap a
data.NatMap.unionsWith : (a ->{g} a ->{g} a) -> [NatMap a] ->{g} NatMap a
data.NatMap.unionWith : (a ->{g} a ->{g} a) -> NatMap a -> NatMap a ->{g} NatMap a
data.NatMap.unionWithKey : (Nat ->{g} a ->{g} a ->{g} a)
                           -> NatMap a
                           -> NatMap a
                           ->{g} NatMap a
data.NatMap.update : (a ->{g} Optional a) -> Nat -> NatMap a ->{g} NatMap a
data.NatMap.updateGetWithKey : (Nat ->{g} a ->{g} Optional a)
                               -> Nat
                               -> NatMap a
                               ->{g} (Optional a, NatMap a)
data.NatMap.updateMax : (a ->{g} a) -> NatMap a ->{g, Abort} NatMap a
data.NatMap.updateMaxWithKey : (Nat ->{g} a ->{g} a) -> NatMap a ->{g, Abort} NatMap a
data.NatMap.updateMin : (a ->{g} a) -> NatMap a ->{g, Abort} NatMap a
data.NatMap.updateMinWithKey : (Nat ->{g} a ->{g} a) -> NatMap a ->{g, Abort} NatMap a
data.NatMap.updateWithKey : (Nat ->{g} a ->{g} Optional a)
                            -> Nat
                            -> NatMap a
                            ->{g} NatMap a
data.NatMap.values : NatMap a -> [a]
type data.NatSet
data.NatSet.== : NatSet -> NatSet -> Boolean
data.NatSet.all : (Nat ->{g} Boolean) ->{g} NatSet ->{g} Boolean
data.NatSet.alter : (Boolean ->{g} Boolean) -> Nat -> NatSet ->{g} NatSet
data.NatSet.any : (Nat ->{g} Boolean) ->{g} NatSet ->{g} Boolean
data.NatSet.contains : Nat -> NatSet -> Boolean
data.NatSet.delete : Nat -> NatSet -> NatSet
data.NatSet.deleteMax : NatSet -> NatSet
data.NatSet.deleteMin : NatSet -> NatSet
data.NatSet.difference : NatSet -> NatSet -> NatSet
data.NatSet.disjoint : NatSet -> NatSet -> Boolean
data.NatSet.empty : NatSet
data.NatSet.equals : NatSet -> NatSet -> Boolean
data.NatSet.filter : (Nat ->{g} Boolean) -> NatSet ->{g} NatSet
data.NatSet.filterMap : (Nat ->{g} Optional Nat) -> NatSet ->{g} NatSet
data.NatSet.foldLeft : (a ->{g} Nat ->{g} a) -> a ->{g} NatSet ->{g} a
data.NatSet.foldMap : (a ->{g} a ->{g} a) -> (Nat ->{g} a) -> a -> NatSet ->{g} a
data.NatSet.foldRight : (Nat ->{g} a ->{g} a) -> a ->{g} NatSet ->{g} a
data.NatSet.fromList : [Nat] -> NatSet
data.NatSet.getAbove : Nat -> NatSet -> Optional Nat
data.NatSet.getAtLeast : Nat -> NatSet -> Optional Nat
data.NatSet.getAtMost : Nat -> NatSet -> Optional Nat
data.NatSet.getBelow : Nat -> NatSet -> Optional Nat
data.NatSet.getMax : NatSet -> Optional Nat
data.NatSet.getMin : NatSet -> Optional Nat
data.NatSet.insert : Nat -> NatSet -> NatSet
data.NatSet.insert.nonempty : Nat -> NatSet -> NatSet.Nonempty
data.NatSet.insert.nonempty.nonempty : Nat -> NatSet -> NatSet.Nonempty
data.NatSet.internal.bim : Nat
                           -> Nat
                           -> NatSet.Nonempty
                           -> NatSet.Nonempty
                           -> NatSet.Nonempty
data.NatSet.internal.bin : Nat -> Nat -> NatSet -> NatSet -> NatSet
data.NatSet.internal.bitmapOf : Nat -> Nat
data.NatSet.internal.bitPred : (Nat ->{g} Boolean) -> Nat -> Nat -> Nat ->{g} Nat
data.NatSet.internal.foldBitsLeft : Nat -> (a ->{g} Nat ->{g} a) -> a -> Nat ->{g} a
data.NatSet.internal.foldBitsRight : Nat -> (Nat ->{g} a ->{g} a) -> a ->{g} Nat ->{g} a
data.NatSet.internal.highBit : Nat -> Nat
data.NatSet.internal.intersectBitmap : Nat -> Nat -> NatSet.Nonempty -> NatSet
data.NatSet.internal.link : Nat
                            -> NatSet.Nonempty
                            -> Nat
                            -> NatSet.Nonempty
                            -> NatSet.Nonempty
data.NatSet.internal.linkWithMask : Nat
                                    -> Nat
                                    -> NatSet.Nonempty
                                    -> NatSet.Nonempty
                                    -> NatSet.Nonempty
data.NatSet.internal.lowestBitMask : Nat -> Nat
data.NatSet.internal.prefixOf : Nat -> Nat
data.NatSet.internal.suffixOf : Nat -> Nat
data.NatSet.internal.tip : Nat -> Nat -> NatSet
data.NatSet.intersect : NatSet -> NatSet -> NatSet
data.NatSet.isEmpty : NatSet -> Boolean
data.NatSet.map : (Nat ->{g} Nat) -> NatSet ->{g} NatSet
data.NatSet.maxView : NatSet ->{Abort} (Nat, NatSet)
data.NatSet.minView : NatSet ->{Abort} (Nat, NatSet)
data.NatSet.NatSet : Optional NatSet.Nonempty -> NatSet
type data.NatSet.Nonempty
data.NatSet.Nonempty.== : NatSet.Nonempty -> NatSet.Nonempty -> Boolean
data.NatSet.Nonempty.alter : (Boolean ->{g} Boolean) -> Nat -> NatSet.Nonempty ->{g} NatSet
data.NatSet.Nonempty.Bin : Nat
                           -> Nat
                           -> Nat
                           -> NatSet.Nonempty
                           -> NatSet.Nonempty
                           -> NatSet.Nonempty
data.NatSet.Nonempty.compare : NatSet.Nonempty -> NatSet.Nonempty -> Ordering
data.NatSet.Nonempty.contains : Nat -> NatSet.Nonempty -> Boolean
data.NatSet.Nonempty.delete : Nat -> NatSet.Nonempty -> NatSet
data.NatSet.Nonempty.deleteMax : NatSet.Nonempty -> NatSet
data.NatSet.Nonempty.deleteMin : NatSet.Nonempty -> NatSet
data.NatSet.Nonempty.difference : NatSet.Nonempty -> NatSet.Nonempty -> NatSet
data.NatSet.Nonempty.disjoint : NatSet.Nonempty -> NatSet.Nonempty -> Boolean
data.NatSet.Nonempty.equals : NatSet.Nonempty -> NatSet.Nonempty -> Boolean
data.NatSet.Nonempty.filter : (Nat ->{g} Boolean) -> NatSet.Nonempty ->{g} NatSet
data.NatSet.Nonempty.filterMap : (Nat ->{g} Optional Nat)
                                 ->{g} NatSet.Nonempty
                                 ->{g} NatSet
data.NatSet.Nonempty.foldLeft : (a ->{g} Nat ->{g} a) -> a ->{g} NatSet.Nonempty ->{g} a
data.NatSet.Nonempty.foldMap : (a ->{g} a ->{g} a)
                               -> (Nat ->{g} a)
                               -> NatSet.Nonempty
                               ->{g} a
data.NatSet.Nonempty.foldRight : (Nat ->{g} a ->{g} a) -> a ->{g} NatSet.Nonempty ->{g} a
data.NatSet.Nonempty.fromList : List.Nonempty Nat -> NatSet.Nonempty
data.NatSet.Nonempty.getAbove : Nat -> NatSet.Nonempty -> Optional Nat
data.NatSet.Nonempty.getAtLeast : Nat -> NatSet.Nonempty -> Optional Nat
data.NatSet.Nonempty.getAtMost : Nat -> NatSet.Nonempty -> Optional Nat
data.NatSet.Nonempty.getBelow : Nat -> NatSet.Nonempty -> Optional Nat
data.NatSet.Nonempty.getMax : NatSet.Nonempty -> Nat
data.NatSet.Nonempty.getMin : NatSet.Nonempty -> Nat
data.NatSet.Nonempty.insert : Nat -> NatSet.Nonempty -> NatSet.Nonempty
data.NatSet.Nonempty.internal.deleteBitmap : Nat -> Nat -> NatSet.Nonempty -> NatSet
data.NatSet.Nonempty.internal.insertBitmap : Nat
                                             -> Nat
                                             -> NatSet.Nonempty
                                             -> NatSet.Nonempty
data.NatSet.Nonempty.intersect : NatSet.Nonempty -> NatSet.Nonempty -> NatSet
data.NatSet.Nonempty.map : (Nat ->{g} Nat) -> NatSet.Nonempty ->{g} NatSet.Nonempty
data.NatSet.Nonempty.maxView : NatSet.Nonempty -> (Nat, NatSet)
data.NatSet.Nonempty.minView : NatSet.Nonempty -> (Nat, NatSet)
data.NatSet.Nonempty.nth : Nat -> NatSet.Nonempty -> Optional Nat
data.NatSet.Nonempty.ordering : NatSet.Nonempty -> NatSet.Nonempty -> Ordering
data.NatSet.Nonempty.partition : (Nat ->{g} Boolean)
                                 -> NatSet.Nonempty
                                 ->{g} (NatSet, NatSet)
data.NatSet.Nonempty.properSubset : NatSet.Nonempty -> NatSet.Nonempty -> Boolean
data.NatSet.Nonempty.properSuperset : NatSet.Nonempty -> NatSet.Nonempty -> Boolean
data.NatSet.Nonempty.randomChoice : NatSet.Nonempty ->{Random} Nat
data.NatSet.Nonempty.singleton : Nat -> NatSet.Nonempty
data.NatSet.Nonempty.size : NatSet.Nonempty -> Nat
data.NatSet.Nonempty.split : Nat -> NatSet.Nonempty -> (NatSet, NatSet)
data.NatSet.Nonempty.splitContains : Nat -> NatSet.Nonempty -> (NatSet, Boolean, NatSet)
data.NatSet.Nonempty.subset : NatSet.Nonempty -> NatSet.Nonempty -> Boolean
data.NatSet.Nonempty.subsetCompare : NatSet.Nonempty
                                     -> NatSet.Nonempty
                                     -> Optional Ordering
data.NatSet.Nonempty.superset : NatSet.Nonempty -> NatSet.Nonempty -> Boolean
data.NatSet.Nonempty.Tip : Nat -> Nat -> NatSet.Nonempty
data.NatSet.Nonempty.tips : NatSet.Nonempty -> List.Nonempty NatSet.Nonempty
data.NatSet.Nonempty.toList : NatSet.Nonempty -> List.Nonempty Nat
data.NatSet.Nonempty.toListAscending : NatSet.Nonempty -> List.Nonempty Nat
data.NatSet.Nonempty.toListDescending : NatSet.Nonempty -> List.Nonempty Nat
data.NatSet.Nonempty.toNatSet : NatSet.Nonempty -> NatSet
data.NatSet.Nonempty.union : NatSet.Nonempty -> NatSet.Nonempty -> NatSet.Nonempty
data.NatSet.Nonempty.unions : List.Nonempty NatSet.Nonempty -> NatSet.Nonempty
data.NatSet.ordering : NatSet -> NatSet -> Ordering
data.NatSet.partition : (Nat ->{g} Boolean) -> NatSet ->{g} (NatSet, NatSet)
data.NatSet.properSubset : NatSet -> NatSet -> Boolean
data.NatSet.properSuperset : NatSet -> NatSet -> Boolean
data.NatSet.randomChoice : NatSet ->{Exception, Random} Nat
data.NatSet.singleton : Nat -> NatSet.Nonempty
data.NatSet.size : NatSet -> Nat
data.NatSet.split : Nat -> NatSet -> (NatSet, NatSet)
data.NatSet.splitContains : Nat -> NatSet -> (NatSet, Boolean, NatSet)
data.NatSet.subset : NatSet -> NatSet -> Boolean
data.NatSet.subsetCompare : NatSet -> NatSet -> Optional Ordering
data.NatSet.superset : NatSet -> NatSet -> Boolean
data.NatSet.takeMax : Nat -> NatSet -> NatSet
data.NatSet.takeMin : Nat -> NatSet -> NatSet
data.NatSet.tips : NatSet -> [NatSet.Nonempty]
data.NatSet.toList : NatSet -> [Nat]
data.NatSet.toListAscending : NatSet -> [Nat]
data.NatSet.toListDescending : NatSet -> [Nat]
data.NatSet.union : NatSet -> NatSet -> NatSet
data.NatSet.unions : [NatSet] -> NatSet
structural type data.OneOrBoth a b
data.OneOrBoth.Both : a -> b -> OneOrBoth a b
data.OneOrBoth.fold : (a ->{e} c)
                      -> (b ->{f} c)
                      -> (a ->{g} b ->{h} c)
                      -> OneOrBoth a b
                      ->{e, f, g, h} c
data.OneOrBoth.isBoth : OneOrBoth a b -> Boolean
data.OneOrBoth.isThat : OneOrBoth a b -> Boolean
data.OneOrBoth.isThis : OneOrBoth a b -> Boolean
data.OneOrBoth.joinThat : (b ->{g1} b ->{g} b)
                          -> OneOrBoth (OneOrBoth a b) b
                          ->{g1, g} OneOrBoth a b
data.OneOrBoth.joinThis : (a ->{g1} a ->{g} a)
                          -> OneOrBoth a (OneOrBoth a b)
                          ->{g1, g} OneOrBoth a b
data.OneOrBoth.justBoth : OneOrBoth a b -> Optional (a, b)
data.OneOrBoth.justThat : OneOrBoth a b -> Optional b
data.OneOrBoth.justThese : [OneOrBoth a b] -> [a]
data.OneOrBoth.justThis : OneOrBoth a b -> Optional a
data.OneOrBoth.justThose : [OneOrBoth a b] -> [b]
data.OneOrBoth.mapBoth : (a ->{f} c) -> (b ->{g} d) -> OneOrBoth a b ->{f, g} OneOrBoth c d
data.OneOrBoth.mapThat : (b ->{g} d) -> OneOrBoth a b ->{g} OneOrBoth a d
data.OneOrBoth.mapThis : (a ->{g} c) -> OneOrBoth a b ->{g} OneOrBoth c b
data.OneOrBoth.maybeThat : OneOrBoth a b -> Optional b
data.OneOrBoth.maybeThis : OneOrBoth a b -> Optional a
data.OneOrBoth.merge : (a ->{f} a ->{g} a) -> OneOrBoth a a ->{f, g} a
data.OneOrBoth.partition : [OneOrBoth a b] -> ([a], [b], [(a, b)])
data.OneOrBoth.That : b -> OneOrBoth a b
data.OneOrBoth.these : [OneOrBoth a b] -> [a]
data.OneOrBoth.This : a -> OneOrBoth a b
data.OneOrBoth.those : [OneOrBoth a b] -> [b]
data.OneOrBoth.toTuple : OneOrBoth a b -> a -> b -> (a, b)
structural type data.RoseTree a
data.RoseTree.flatten : RoseTree a -> [a]
data.RoseTree.flattens : [a] -> [RoseTree a] -> [a]
data.RoseTree.Node : a -> [RoseTree a] -> RoseTree a
structural type data.SeqView a b
data.SeqView.VElem : a -> b -> SeqView a b
data.SeqView.VEmpty : SeqView a b
structural type data.Set a
data.Set.== : Set k -> Set k -> Boolean
data.Set.all : (a ->{e} Boolean) -> Set a ->{e} Boolean
data.Set.any : (a ->{e} Boolean) -> Set a ->{e} Boolean
data.Set.contains : k -> Set k -> Boolean
data.Set.delete : k -> Set k -> Set k
data.Set.deletes : [a] -> Set a -> Set a
data.Set.difference : Set a -> Set a -> Set a
data.Set.elementAt : Nat -> Set a -> Optional a
data.Set.empty : Set k
data.Set.equals : Set k -> Set k -> Boolean
data.Set.flatMap : (i ->{g} Set k) -> Set i ->{g} Set k
data.Set.flatten : Set (Set k) -> Set k
data.Set.foldLeft : (b ->{e} a ->{e} b) -> b -> Set a ->{e} b
data.Set.foldRight : (a ->{e} b ->{e} b) -> b -> Set a ->{e} b
data.Set.fromList : [k] -> Set k
data.Set.fromText : Text -> Set Char
data.Set.insert : k -> Set k -> Set k
data.Set.insertNonempty : k -> Set k -> Set.Nonempty k
data.Set.internal.Set : Map a () -> Set a
data.Set.internal.underlying : Set k -> Map k ()
data.Set.intersect : Set k -> Set k -> Set k
data.Set.intersects : [Set a] -> Set a
data.Set.isEmpty : Set k -> Boolean
data.Set.map : (a ->{e} b) -> Set a ->{e} Set b
structural type data.Set.Nonempty a
data.Set.Nonempty.== : Set.Nonempty k -> Set.Nonempty k -> Boolean
data.Set.Nonempty.all : (a ->{e} Boolean) -> Set.Nonempty a ->{e} Boolean
data.Set.Nonempty.any : (a ->{e} Boolean) -> Set.Nonempty a ->{e} Boolean
data.Set.Nonempty.contains : k -> Set.Nonempty k -> Boolean
data.Set.Nonempty.delete : k -> Set.Nonempty k -> Set k
data.Set.Nonempty.deletes : [a] -> Set.Nonempty a -> Set a
data.Set.Nonempty.elementAt : Nat -> Set.Nonempty a -> Optional a
data.Set.Nonempty.equals : Set.Nonempty k -> Set.Nonempty k -> Boolean
data.Set.Nonempty.flatMap : (i ->{g} Set.Nonempty k) -> Set.Nonempty i ->{g} Set.Nonempty k
data.Set.Nonempty.flatten : Set.Nonempty (Set.Nonempty k) -> Set.Nonempty k
data.Set.Nonempty.foldLeft : (b ->{e} a ->{e} b) -> b -> Set.Nonempty a ->{e} b
data.Set.Nonempty.foldMap : (b ->{e} b ->{e} b) -> (a ->{e} b) -> Set.Nonempty a ->{e} b
data.Set.Nonempty.foldRight : (a ->{e} b ->{e} b) -> b -> Set.Nonempty a ->{e} b
data.Set.Nonempty.fromList : List.Nonempty k -> Set.Nonempty k
data.Set.Nonempty.fromListAnd : k -> [k] -> Set.Nonempty k
data.Set.Nonempty.fromTextAnd : Char -> Text -> Set.Nonempty Char
data.Set.Nonempty.insert : k -> Set.Nonempty k -> Set.Nonempty k
data.Set.Nonempty.internal.underlying : Set.Nonempty k -> Map.Nonempty k ()
data.Set.Nonempty.intersect : Set.Nonempty k -> Set.Nonempty k -> Set k
data.Set.Nonempty.intersects : [Set.Nonempty a] -> Set a
data.Set.Nonempty.map : (a ->{e} b) -> Set.Nonempty a ->{e} Set.Nonempty b
data.Set.Nonempty.random : Set.Nonempty a ->{Random} a
data.Set.Nonempty.randomChoice : Set.Nonempty a ->{Random} a
data.Set.Nonempty.reduce : (a ->{e} a ->{e} a) -> Set.Nonempty a ->{e} a
data.Set.Nonempty.Set : Map.Nonempty a () -> Set.Nonempty a
data.Set.Nonempty.similarity : Set.Nonempty k -> Set.Nonempty k -> Float
data.Set.Nonempty.singleton : a -> Set.Nonempty a
data.Set.Nonempty.size : Set.Nonempty k -> Nat
data.Set.Nonempty.subset : Set.Nonempty a -> Set.Nonempty a -> Boolean
data.Set.Nonempty.superset : Set.Nonempty a -> Set.Nonempty a -> Boolean
data.Set.Nonempty.toList : Set.Nonempty k -> List.Nonempty k
data.Set.Nonempty.toMap : (k ->{e} v) -> Set.Nonempty k ->{e} Map.Nonempty k v
data.Set.Nonempty.toSet : Set.Nonempty k -> Set k
data.Set.Nonempty.toText : Set.Nonempty Char -> Text
data.Set.Nonempty.union : Set.Nonempty k -> Set.Nonempty k -> Set.Nonempty k
data.Set.Nonempty.unions : List.Nonempty (Set.Nonempty a) -> Set.Nonempty a
data.Set.random : Set a ->{Random} Optional a
data.Set.randomChoice : Set a ->{Exception, Random} a
data.Set.similarity : Set k -> Set k -> Float
data.Set.singleton : a -> Set a
data.Set.size : Set k -> Nat
data.Set.subset : Set a -> Set a -> Boolean
data.Set.superset : Set a -> Set a -> Boolean
data.Set.toList : Set k -> [k]
data.Set.toMap : (k ->{e} v) -> Set k ->{e} Map k v
data.Set.toText : Set Char -> Text
data.Set.union : Set k -> Set k -> Set k
data.Set.unions : [Set a] -> Set a
structural ability data.Stream e
data.Stream.++ : (v1 ->{g, Stream a} v2)
                 -> (v2 ->{g, Stream a} v3)
                 -> v1
                 ->{g, Stream a} v3
data.Stream.+: : a -> '{g, Stream a} r -> '{g, Stream a} r
data.Stream.all : (a ->{g} Boolean) -> '{g, Stream a} r ->{g} Boolean
data.Stream.any : (a ->{g} Boolean) -> '{g, Stream a} r ->{g} Boolean
data.Stream.changes : Boolean -> '{g, Stream a} r -> '{g, Stream a} r
data.Stream.changes! : Boolean -> '{g, Stream a} r ->{g, Stream a} r
data.Stream.changesBy : (a -> a -> Boolean)
                        -> Boolean
                        -> '{g, Stream a} r
                        -> '{g, Stream a} r
data.Stream.changesBy! : (a -> a -> Boolean)
                         -> Boolean
                         -> '{g, Stream a} r
                         ->{g, Stream a} r
data.Stream.chunk : Nat -> '{g, Stream a} r -> '{g, Stream (List.Nonempty a)} r
data.Stream.chunk! : Nat -> '{g, Stream a} r ->{g, Stream (List.Nonempty a)} r
data.Stream.collate : '{g1, Stream a} r -> '{g2, Stream a} Void -> '{g1, g2, Stream a} r
ability data.Stream.collate.test.Counter
data.Stream.collate.test.Counter.comma! : {Counter} ()
data.Stream.collate.test.Counter.nat! : {Counter} ()
data.Stream.collate! : '{g1, Stream a} r -> '{g2, Stream a} Void ->{g1, g2, Stream a} r
data.Stream.collate!.handler1 : '{g2} Void -> Request (Stream a) r ->{g2, Stream a} r
data.Stream.collate!.handler2 : a -> '{g2} r -> Request (Stream a) Void ->{g2, Stream a} r
data.Stream.collate!.handler3 : '{g2} Void -> Request (Stream a) r ->{g2, Stream a} r
data.Stream.concatMap : (a ->{e} [b]) -> '{e, Stream a} r -> '{e, Stream b} r
data.Stream.concatMap! : (a ->{e} [b]) -> '{e, Stream a} r ->{e, Stream b} r
data.Stream.contains : a -> '{g, Stream a} r ->{g} Boolean
data.Stream.drain : '{g, Stream a} r ->{g} r
data.Stream.drop : Nat -> '{g, Stream a} r -> '{g, Stream a} r
data.Stream.drop! : Nat -> '{g, Stream a} r ->{g, Stream a} r
data.Stream.dropWhile : (a ->{e} Boolean) -> '{f, Stream a} r -> '{e, f, Stream a} r
data.Stream.dropWhile! : (a ->{e} Boolean) -> '{f, Stream a} r ->{e, f, Stream a} r
data.Stream.emit : e ->{Stream e} ()
data.Stream.emitAndReturn : a -> '{Stream a} a
data.Stream.emitAndReturn! : a ->{Stream a} a
data.Stream.filter : (a ->{g} Boolean) -> '{g, Stream a} r -> '{g, Stream a} r
data.Stream.filter! : (a ->{g} Boolean) -> '{g, Stream a} r ->{g, Stream a} r
data.Stream.filterMap : (a ->{g} Optional b) -> '{g, Stream a} r -> '{g, Stream b} r
data.Stream.filterMap! : (a ->{g} Optional b) -> '{g, Stream a} r ->{g, Stream b} r
data.Stream.find : (a ->{g} Boolean) -> '{g, Stream a} r ->{g} Optional a
data.Stream.flatMap : (a ->{g, Stream b} any) -> '{g, Stream a} r -> '{g, Stream b} r
data.Stream.flatMap! : (a ->{g, Stream b} any) -> '{g, Stream a} r ->{g, Stream b} r
data.Stream.fold : (b ->{g} a ->{g} b) -> b -> '{g, Stream a} r ->{g} b
data.Stream.foldBalanced : (a ->{g1} b)
                           -> b
                           -> (b -> b ->{g2} b)
                           -> '{g3, Stream a} r
                           ->{g1, g2, g3} b
data.Stream.foldDelayed : (b ->{g} a ->{g} b) -> b -> '{g, Stream a} r -> '{g} b
data.Stream.foldDelayedRight : (a ->{g} b ->{g} b) -> b -> '{g, Stream a} r -> '{g} b
data.Stream.foldDelayedWithResult : (b ->{g} a ->{g} b)
                                    -> b
                                    -> '{g, Stream a} r
                                    -> '{g} (b, r)
data.Stream.foldRight : (a ->{g} b ->{g} b) -> b -> '{g, Stream a} r ->{g} b
data.Stream.foldWithResult : (b ->{g} a ->{g} b) -> b -> '{g, Stream a} r ->{g} (b, r)
data.Stream.foldWithResult.handler : (b ->{g} a ->{g} b)
                                     -> b
                                     -> Request (Stream a) r
                                     ->{g} (b, r)
data.Stream.foreach : (a ->{g} ()) -> '{g, Stream a} r ->{g} r
data.Stream.from : Nat -> '{Stream Nat} a
data.Stream.from! : Nat ->{Stream Nat} a
data.Stream.fromList : [a] -> '{Stream a} ()
data.Stream.fromList! : [a] ->{Stream a} ()
data.Stream.head : '{g, Stream a} r ->{g} Optional a
data.Stream.indexed : '{g, Stream a} r -> '{g, Stream (a, Nat)} r
data.Stream.indexed! : '{g, Stream a} r ->{g, Stream (a, Nat)} r
data.Stream.Int.all : '{Stream Int} a
data.Stream.Int.from : Int -> '{Stream Int} a
data.Stream.Int.from! : Int ->{Stream Int} a
data.Stream.Int.negatives : '{Stream Int} a
data.Stream.Int.positives : '{Stream Int} a
data.Stream.Int.range : Int -> Int -> '{Stream Int} ()
data.Stream.Int.range! : Int ->{Stream Int} Int ->{Stream Int} ()
data.Stream.interleave : '{g, Stream a} r -> '{g, Stream a} r -> '{g, Stream a} r
data.Stream.interleave! : '{g, Stream a} r -> '{g, Stream a} r ->{g, Stream a} r
data.Stream.interleave!.handler : '{e} r -> Request (Stream a) r ->{e, Stream a} r
data.Stream.intersperse : a -> '{g, Stream a} r -> '{g, Stream a} r
data.Stream.intersperse! : a -> '{g, Stream a} r ->{g, Stream a} r
data.Stream.iterate : (a ->{g} a) -> a -> '{g, Stream a} Void
data.Stream.iterate! : (a ->{g} a) -> a ->{g, Stream a} Void
data.Stream.map : (a ->{g} b) -> '{g, Stream a} r -> '{g, Stream b} r
data.Stream.map! : (a ->{g} b) -> '{g, Stream a} r ->{g, Stream b} r
data.Stream.Nat.all : '{Stream Nat} a
data.Stream.pipe : '{g, Stream a} r -> '{g, Ask a, Stream b} r -> '{g, Stream b} r
data.Stream.pipe! : '{g, Stream a} r -> '{g, Ask a, Stream b} r ->{g, Stream b} r
data.Stream.pipe!.handler : '{g, Stream a} r
                            -> Request {Ask a, Stream b} r
                            ->{g, Stream b} r
data.Stream.range : Nat -> Nat -> '{Stream Nat} ()
data.Stream.range! : Nat -> Nat ->{Stream Nat} ()
data.Stream.rangeClosed : Nat -> Nat -> '{Stream Nat} ()
data.Stream.rangeClosed! : Nat -> Nat ->{Stream Nat} ()
data.Stream.repeat : '{g} a -> '{g, Stream a} Void
data.Stream.scan : (b ->{g} a ->{g} b) -> b -> '{g, Stream a} r -> '{g, Stream b} r
data.Stream.scan! : (b ->{g} a ->{g} b) -> b -> '{g, Stream a} r ->{g, Stream b} r
data.Stream.somes : '{g, Stream (Optional a)} r -> '{g, Stream a} r
data.Stream.somes! : '{g, Stream (Optional a)} r ->{g, Stream a} r
data.Stream.span : (a ->{g} Boolean) -> '{g, Stream a} r ->{g} ([a], '{g, Stream a} r)
data.Stream.splitAt : Nat -> '{g, Stream a} r ->{g} ([a], '{g, Stream a} r)
data.Stream.tails : '{g, Stream a} r -> '{g, Stream ('{Stream a, g} r)} r
data.Stream.tails! : '{g, Stream a} r ->{g, Stream ('{Stream a, g} r)} r
data.Stream.take : Nat -> '{g, Stream a} t -> '{g, Stream a} Optional t
data.Stream.take! : Nat -> '{g, Stream a} r ->{g, Stream a} Optional r
data.Stream.takeWhile : (a ->{g} Boolean) -> '{g, Stream a} r -> '{g, Stream a} Optional r
data.Stream.takeWhile! : (a ->{g} Boolean) -> '{g, Stream a} r ->{g, Stream a} Optional r
data.Stream.tap : (x ->{g1} ()) -> '{g2, Stream x} r -> '{g1, g2, Stream x} r
data.Stream.tap! : (x ->{g1} ()) -> '{g2, Stream x} r ->{g1, g2, Stream x} r
data.Stream.terminated : '{g, Stream a} r -> '{g, Stream (Optional a)} r
data.Stream.terminated! : '{g, Stream a} r ->{g, Stream (Optional a)} r
data.Stream.to : Nat -> '{Stream Nat} ()
data.Stream.to! : Nat ->{Stream Nat} ()
data.Stream.toDelayedList : '{g, Stream a} r -> '{g} [a]
data.Stream.toDelayedList.handler : Request {Stream a} () -> [a]
data.Stream.toDelayedListWithResult : '{g, Stream a} r -> '{g} ([a], r)
data.Stream.toList : '{g, Stream a} r ->{g} [a]
data.Stream.toListWithResult : '{g, Stream a} r ->{g} ([a], r)
data.Stream.trace : Text -> '{g, Stream a} r ->{g} r
data.Stream.uncons : '{g, Stream a} r ->{g} Either r (a, '{g, Stream a} r)
data.Stream.unfold : s -> (s ->{g} Optional (a, s)) -> '{g, Stream a} ()
data.Stream.until : Nat -> '{Stream Nat} ()
data.Stream.until! : Nat ->{Stream Nat} ()
data.Stream.window : Nat -> '{g, Stream a} r -> '{g, Stream [a]} r
data.Stream.window! : Nat -> '{g, Stream a} r ->{g, Stream [a]} r
data.Stream.zip : '{g, Stream a} r -> '{g, Stream b} r -> '{g, Stream (a, b)} r
data.Stream.zip! : '{g, Stream a} r -> '{g, Stream b} r ->{g, Stream (a, b)} r
data.Stream.zipWith : (a ->{g} b ->{g} c)
                      -> '{g, Stream a} r
                      -> '{g, Stream b} r
                      -> '{g, Stream c} r
data.Stream.zipWith! : (a ->{g} b ->{g} c)
                       -> '{g, Stream a} r
                       -> '{g, Stream b} r
                       ->{g, Stream c} r
structural type data.Trie k v
data.Trie.empty : Trie k v
data.Trie.fromList : [([k], v)] -> Trie k v
data.Trie.gen : '{Gen} k -> '{Gen} v -> '{Gen} Trie k v
data.Trie.head : Trie k v -> Optional v
data.Trie.head.modify : (Optional v ->{g} Optional v) -> Trie k v ->{g} Trie k v
data.Trie.head.set : Optional v -> Trie k v -> Trie k v
data.Trie.insert : [k] -> v -> Trie k v -> Trie k v
data.Trie.isEmpty : Trie k v -> Boolean
data.Trie.lookup : [k] -> Trie k v -> Optional v
data.Trie.map : (v1 ->{e} v2) ->{e} Trie k v1 ->{e} Trie k v2
data.Trie.mapKeys : (k1 ->{e} k2) ->{e} Trie k1 v ->{e} Trie k2 v
data.Trie.singleton : [k] -> v -> Trie k v
data.Trie.subtrie : [k] -> Trie k v -> Trie k v
data.Trie.tail : Trie k v -> Map k (Trie k v)
data.Trie.tail.modify : (Map k111 (Trie k111 v) ->{g} Map k (Trie k v))
                        -> Trie k111 v
                        ->{g} Trie k v
data.Trie.tail.set : Map k (Trie k v) -> Trie k111 v -> Trie k v
data.Trie.text.fromList : [(Text, v)] -> Trie Char v
data.Trie.text.insert : Text -> v -> Trie Char v -> Trie Char v
data.Trie.text.lookup : Text -> Trie Char v -> Optional v
data.Trie.text.toList : Trie Char v -> [(Text, v)]
data.Trie.toList : Trie k v -> [([k], v)]
data.Trie.toMap : Trie k v -> Map [k] v
data.Trie.Trie : Optional v -> Map k (Trie k v) -> Trie k v
data.Trie.union : Trie k v -> Trie k v -> Trie k v
data.Trie.unionWith : (v ->{e} v ->{e} v) ->{e} Trie k v ->{e} Trie k v ->{e} Trie k v
data.Trie.values : Trie k v -> [v]
structural type data.Tuple a b
data.Tuple.at1 : Tuple a b -> a
data.Tuple.at2 : Tuple a (Tuple b c) -> b
data.Tuple.at3 : Tuple a (Tuple b (Tuple c d)) -> c
data.Tuple.at4 : Tuple a (Tuple b (Tuple c (Tuple d e))) -> d
data.Tuple.bimap : (a ->{g1} b) -> (a, a) ->{g1} (b, b)
data.Tuple.Cons : a -> b -> Tuple a b
data.Tuple.first : (i ->{g} o) -> Tuple i b ->{g} Tuple o b
data.Tuple.mapLeft : (a ->{g} b) -> (a, c) ->{g} (b, c)
data.Tuple.mapPair : ((a ->{g} b), (c ->{g} d)) -> (a, c) ->{g} (b, d)
data.Tuple.mapRight : (a ->{g} b) -> (c, a) ->{g} (c, b)
data.Tuple.pair : a -> b -> (a, b)
data.Tuple.second : (i ->{g} o) -> Tuple a (Tuple i b) ->{g} Tuple a (Tuple o b)
data.Tuple.swap : (a, b) -> (b, a)
Debug.tap : Text -> a -> a
Debug.toDebugText : a -> Text
Debug.toDebugText.impl : a -> Optional (Either Text Text)
Debug.trace : Text -> a -> ()
Debug.watch : Text -> a -> a
Doc.Deprecated.++ : Deprecated -> Deprecated -> Deprecated
Doc.Deprecated.Blob : Text -> Deprecated
Doc.Deprecated.Evaluate : Link.Term -> Deprecated
Doc.Deprecated.example : Link.Term -> Deprecated
Doc.Deprecated.Join : [Deprecated] -> Deprecated
Doc.Deprecated.Link : Link -> Deprecated
Doc.Deprecated.Signature : Link.Term -> Deprecated
Doc.Deprecated.Source : Link -> Deprecated
Doc.EmbedSvg.EmbedSvg : Text -> EmbedSvg
Doc.FrontMatter.FrontMatter : [(Text, Text)] -> FrontMatter
Doc.LaTeXInline.LaTeXInline : Text -> LaTeXInline
Doc.MediaSource.MediaSource : Text -> Optional Text -> MediaSource
Doc.MediaSource.mimeType : MediaSource -> Optional Text
Doc.MediaSource.mimeType.modify : (Optional Text ->{g} Optional Text)
                                  -> MediaSource
                                  ->{g} MediaSource
Doc.MediaSource.mimeType.set : Optional Text -> MediaSource -> MediaSource
Doc.MediaSource.sourceUrl : MediaSource -> Text
Doc.MediaSource.sourceUrl.modify : (Text ->{g} Text) -> MediaSource ->{g} MediaSource
Doc.MediaSource.sourceUrl.set : Text -> MediaSource -> MediaSource
Doc.SpecialForm.Embed : Any -> SpecialForm
Doc.SpecialForm.EmbedInline : Any -> SpecialForm
Doc.Video.config : Video -> [(Text, Text)]
Doc.Video.config.modify : ([(Text, Text)] ->{g} [(Text, Text)]) -> Video ->{g} Video
Doc.Video.config.set : [(Text, Text)] -> Video -> Video
Doc.Video.sources : Video -> [MediaSource]
Doc.Video.sources.modify : ([MediaSource] ->{g} [MediaSource]) -> Video ->{g} Video
Doc.Video.sources.set : [MediaSource] -> Video -> Video
Doc.Video.Video : [MediaSource] -> [(Text, Text)] -> Video
structural type Either a b
Either.bimap : (a ->{g} b) -> (c ->{g} d) -> Either a c ->{g} Either b d
Either.flatMapRight : (a ->{g1} Either e b) -> Either e a ->{g1} Either e b
Either.flattenRight : Either e (Either e a) -> Either e a
Either.fold : (a ->{e} c) -> (b ->{e} c) -> Either a b ->{e} c
Either.isLeft : Either a b -> Boolean
Either.isRight : Either a b -> Boolean
Either.left : Either l r -> Optional l
Either.Left : a -> Either a b
Either.mapLeft : (a ->{g} b) -> Either a z ->{g} Either b z
Either.mapRight : (a ->{g} b) -> Either e a ->{g} Either e b
Either.raiseMessage : v -> Either Text a ->{Exception} a
Either.right : Either l r -> Optional r
Either.Right : b -> Either a b
Either.toAbort : Either a b ->{Abort} b
Either.toBug : Either e a -> a
Either.toException : Either Failure a ->{Exception} a
Either.toOptional : Either a b -> Optional b
Either.toThrow : Either e a ->{Throw e} a
Either.unwrap : Either a a -> a
builtin type Float
Float.!= : Float -> Float -> Boolean
Float.* : Float -> Float -> Float
Float.+ : Float -> Float -> Float
Float.- : Float -> Float -> Float
Float./ : Float -> Float -> Float
Float.< : Float -> Float -> Boolean
Float.<= : Float -> Float -> Boolean
Float.== : Float -> Float -> Boolean
Float.> : Float -> Float -> Boolean
Float.>= : Float -> Float -> Boolean
Float.abs : Float -> Float
Float.acos : Float -> Float
Float.acosh : Float -> Float
Float.asin : Float -> Float
Float.asinh : Float -> Float
Float.atan : Float -> Float
Float.atan2 : Float -> Float -> Float
Float.atanh : Float -> Float
Float.ceiling : Float -> Float
Float.ceiling.deprecated : Float -> Int
Float.clamp : Float -> Float -> Float -> Float
Float.components : Float ->{Throw Text} (Int, Nat)
Float.cos : Float -> Float
Float.cosh : Float -> Float
Float.e : Float
Float.emod : Float -> Float ->{Exception} Float
Float.eq : Float -> Float -> Boolean
Float.equalUpTo : Float -> Float -> Float -> Boolean
Float.exp : Float -> Float
Float.floor : Float -> Float
Float.floor.deprecated : Float -> Int
Float.fromHalfPrecision : Nat -> Float
Float.fromInt : Int -> Float
Float.fromNat : Nat -> Float
Float.fromRepresentation : Nat -> Float
Float.fromSinglePrecision : Nat -> Float
Float.fromText : Text -> Optional Float
Float.gt : Float -> Float -> Boolean
Float.gteq : Float -> Float -> Boolean
Float.Infinity : Float
Float.inRange : Float -> Float -> Float -> Boolean
Float.isInfinity : Float -> Boolean
Float.isNaN : Float -> Boolean
Float.isNegativeInfinity : Float -> Boolean
Float.log : Float -> Float
Float.logBase : Float -> Float -> Float
Float.lt : Float -> Float -> Boolean
Float.lteq : Float -> Float -> Boolean
Float.max : Float -> Float -> Float
Float.maxFloat : Float
Float.min : Float -> Float -> Float
Float.minFloat : Float
Float.mod : Float -> Float ->{Exception} Float
Float.NaN : Float
Float.negate : Float -> Float
Float.NegativeInfinity : Float
Float.neq : Float -> Float -> Boolean
Float.pi : Float
Float.pow : Float -> Float -> Float
Float.product : [Float] -> Float
Float.rawExponent : Float -> Nat
Float.rawMantissa : Float -> Nat
Float.reciprocal : Float -> Float
Float.round : Float -> Float
Float.round.deprecated : Float -> Int
Float.roundTo : Nat -> Float ->{Exception} Float
Float.roundToWith : (Float -> Float) -> Nat -> Float ->{Exception} Float
Float.safeEmod : Float -> Float -> Optional Float
Float.safeMod : Float -> Float -> Optional Float
Float.safeRoundTo : Nat -> Float -> Optional Float
Float.safeRoundToWith : (Float -> Float) -> Nat -> Float -> Optional Float
Float.signBit : Float -> Nat
Float.sin : Float -> Float
Float.sinh : Float -> Float
Float.sqrt : Float -> Float
Float.sum : [Float] -> Float
Float.tan : Float -> Float
Float.tanh : Float -> Float
Float.toHalfPrecision : Float -> Nat
Float.toInt : Float -> Optional Int
Float.toNat : Float -> Optional Nat
Float.toRepresentation : Float -> Nat
Float.toSinglePrecision : Float -> Nat
Float.toText : Float -> Text
Float.truncate : Float -> Float
Float.ulp : Float -> Float
Float.ulpDiff : Float -> Float -> Nat
Float.unsafeToInt : Float -> Int
Float.withinULPs : Nat -> Float -> Float -> Boolean
Function.&&& : (a ->{g} b) -> (a ->{h} c) -> a ->{g, h} (b, c)
Function.<< : (b ->{g} c) -> (a ->{g} b) -> a ->{g} c
Function.<| : (a ->{g} b) -> a ->{g} b
Function.<|| : (r ->{g} a) -> (r ->{e} a ->{f} b) -> r ->{e, f, g} b
Function.>> : (a ->{g} b) -> (b ->{g} c) -> a ->{g} c
Function.andThen : (a ->{g} b) -> (b ->{g} c) -> a ->{g} c
Function.apply2 : (a ->{e} b ->{e} c) -> (r ->{e} a) -> (r ->{e} b) -> r ->{e} c
Function.applyAll : [a ->{e} b] -> a ->{e} [b]
Function.both : (a ->{g} b) -> (a ->{h} c) -> a ->{g, h} (b, c)
Function.compose : (b ->{g} c) -> (a ->{g} b) -> a ->{g} c
Function.compose2 : (c ->{g} d) -> (a ->{g} b ->{g} c) -> a -> b ->{g} d
Function.compose3 : (d ->{f} e)
                    -> (a ->{g} b ->{h} c ->{i} d)
                    -> a
                    -> b
                    -> c
                    ->{f, g, h, i} e
Function.composeK : (b ->{e} r ->{e} c) -> (a ->{e} r ->{e} b) -> a -> r ->{e} c
Function.const : a -> b -> a
Function.curry : ((a, b) ->{e} c) -> a -> b ->{e} c
Function.delay : (a ->{g} b) -> a -> '{g} b
Function.fix : ('{e} a ->{e} a) ->{e} a
Function.fix.examples.factorial.explicitRecursion : Nat
Function.fix.examples.factorial.fixpoint : Nat
Function.flatMap : (a -> r ->{e} b) -> (r ->{e} a) -> r ->{e} b
Function.flip : (a ->{e} b ->{e} c) -> b -> a ->{e} c
Function.id : a -> a
Function.join : (i ->{g} i ->{h} o) -> i ->{g, h} o
Function.loopWhile : (a ->{e} (a, Boolean)) -> a ->{e} a
Function.on : (b ->{e} b ->{e} c) -> (a ->{e} b) -> a -> a ->{e} c
Function.on.examples.equalOn : Boolean
Function.tap : (a ->{g} ()) -> a ->{g} a
Function.times : (a ->{e} a) -> Nat -> a ->{e} a
Function.uncurry : (a ->{e} b ->{e} c) -> (a, b) ->{e} c
Function.|> : a -> (a ->{g} b) ->{g} b
Function.||> : (r ->{e} a ->{f} b) -> (r ->{g} a) -> r ->{e, f, g} b
type GUID
GUID.GUID : Bytes -> GUID
GUID.new : '{Random} GUID
GUID.new.deprecated : Text -> Text -> GUID
GUID.toBase16 : GUID -> Text
GUID.toBytes : GUID -> Bytes
Hash.Murmur.add : Nat -> Nat -> Nat
Hash.Murmur.finish : Nat -> Nat
Hash.Murmur.impl.murmur_m : Nat
Hash.Murmur.impl.murmur_r : Nat
Hash.Murmur.initialSeed : Nat
ignore : a -> ()
builtin type Int
Int.!= : Int -> Int -> Boolean
Int.% : Int -> Int -> Int
Int.* : Int -> Int -> Int
Int.+ : Int -> Int -> Int
Int.- : Int -> Int -> Int
Int./ : Int -> Int -> Int
Int.< : Int -> Int -> Boolean
Int.<= : Int -> Int -> Boolean
Int.== : Int -> Int -> Boolean
Int.> : Int -> Int -> Boolean
Int.>= : Int -> Int -> Boolean
Int.abs : Int -> Nat
Int.and : Int -> Int -> Int
Int.clamp : Int -> Int -> Int -> Int
Int.complement : Int -> Int
Int.decrement : Int -> Int
Int.diff : Int -> Int -> Nat
Int.div : Int -> Int -> Int
Int.div.impl : Int -> Int -> Int
Int.ediv : Int -> Int -> Int
Int.emod : Int -> Int -> Nat
Int.eq : Int -> Int -> Boolean
Int.fromRepresentation : Nat -> Int
Int.fromText : Text -> Optional Int
Int.gcd : Int -> Int ->{Abort} Nat
Int.gt : Int -> Int -> Boolean
Int.gteq : Int -> Int -> Boolean
Int.increment : Int -> Int
Int.inRange : Int -> Int -> Int -> Boolean
Int.isEven : Int -> Boolean
Int.isNegative : Int -> Boolean
Int.isOdd : Int -> Boolean
Int.lcm : Int -> Int ->{Abort} Int
Int.leadingZeros : Int -> Nat
Int.lt : Int -> Int -> Boolean
Int.lteq : Int -> Int -> Boolean
Int.max : Int -> Int -> Int
Int.maxInt : Int
Int.maybeMultiply : Int -> Int ->{Abort} Int
Int.min : Int -> Int -> Int
Int.minInt : Int
Int.mod : Int -> Int -> Int
Int.negate : Int -> Int
Int.neq : Int -> Int -> Boolean
Int.or : Int -> Int -> Int
Int.popCount : Int -> Nat
Int.pow : Int -> Nat -> Int
Int.product : [Int] -> Int
Int.range : Int -> Int -> [Int]
Int.range.examples.invalid.descFromNeg : [Int]
Int.range.examples.invalid.descFromPos : [Int]
Int.range.examples.valid.ascFromNeg : [Int]
Int.range.examples.valid.ascFromNeg2 : [Int]
Int.range.examples.valid.ascFromPos : [Int]
Int.rangeClosed : Int -> Int -> [Int]
Int.safeDiv : Int -> Int -> Optional Int
Int.safeEdiv : Int -> Int -> Optional Int
Int.safeEmod : Int -> Int -> Optional Nat
Int.safeMod : Int -> Int -> Optional Int
Int.shiftLeft : Int -> Nat -> Int
Int.shiftRight : Int -> Nat -> Int
Int.shiftRightL : Int -> Nat -> Int
Int.signum : Int -> Int
Int.sum : [Int] -> Int
Int.toFloat : Int -> Float
Int.toNat : Int -> Optional Nat
Int.toRepresentation : Int -> Nat
Int.toText : Int -> Text
Int.toTextBase : Nat -> Int -> Optional Text
Int.trailingZeros : Int -> Nat
Int.truncate0 : Int -> Nat
Int.xor : Int -> Int -> Int
builtin type IO
IO.arrayOf : a -> Nat ->{IO} mutable.Array {IO} a
IO.byteArray : Nat ->{IO} mutable.ByteArray {IO}
IO.byteArrayOf : Nat -> Nat ->{IO} mutable.ByteArray {IO}
IO.catchAll : '{IO, Exception} a ->{IO} Either Failure a
IO.concurrent.fork : '{IO} () ->{IO} ThreadId
IO.concurrent.fork.impl : '{IO} a ->{IO} ThreadId
IO.concurrent.fork_ : '{IO} () ->{IO} ()
IO.concurrent.kill : ThreadId ->{IO, Exception} ()
IO.concurrent.kill.impl : ThreadId ->{IO} Either Failure ()
builtin type IO.concurrent.MVar
IO.concurrent.MVar.isEmpty : MVar a ->{IO} Boolean
IO.concurrent.MVar.modify : MVar a -> (a ->{IO, Exception} (a, b)) ->{IO, Exception} b
IO.concurrent.MVar.new : a ->{IO} MVar a
IO.concurrent.MVar.newEmpty : '{IO} MVar a
IO.concurrent.MVar.put : MVar a -> a ->{IO, Exception} ()
IO.concurrent.MVar.put.impl : MVar a -> a ->{IO} Either Failure ()
IO.concurrent.MVar.read : MVar a ->{IO, Exception} a
IO.concurrent.MVar.read.impl : MVar a ->{IO} Either Failure a
IO.concurrent.MVar.swap : MVar a -> a ->{IO, Exception} a
IO.concurrent.MVar.swap.impl : MVar a -> a ->{IO} Either Failure a
IO.concurrent.MVar.take : MVar a ->{IO, Exception} a
IO.concurrent.MVar.take.impl : MVar a ->{IO} Either Failure a
IO.concurrent.MVar.tryModify : MVar a
                               -> (a ->{IO, Exception} (a, b))
                               ->{IO, Exception} Optional b
IO.concurrent.MVar.tryPut : MVar a -> a ->{IO, Exception} Boolean
IO.concurrent.MVar.tryPut.impl : MVar a -> a ->{IO} Either Failure Boolean
IO.concurrent.MVar.tryRead : MVar a ->{IO, Exception} Optional a
IO.concurrent.MVar.tryRead.impl : MVar a ->{IO} Either Failure (Optional a)
IO.concurrent.MVar.tryTake : MVar a ->{IO} Optional a
builtin type IO.concurrent.Promise
IO.concurrent.Promise.new : '{IO} Promise a
IO.concurrent.Promise.read : Promise a ->{IO} a
IO.concurrent.Promise.tryRead : Promise a ->{IO} Optional a
IO.concurrent.Promise.write : Promise a -> a ->{IO} Boolean
IO.concurrent.Promise.write_ : Promise a -> a ->{IO} ()
IO.concurrent.sleep : Duration ->{IO, Exception} ()
IO.concurrent.sleepMicroseconds : Nat ->{IO, Exception} ()
IO.concurrent.sleepMicroseconds.examples.ex1 : '{IO, Exception} ()
IO.concurrent.sleepMicroseconds.impl : Nat ->{IO} Either Failure ()
builtin type IO.concurrent.STM
IO.concurrent.STM.atomically : '{STM} a ->{IO} a
IO.concurrent.STM.retry : '{STM} a
type IO.concurrent.STM.STMFailure
type IO.concurrent.STM.TMap a
IO.concurrent.STM.TMap.contains : Bytes -> TMap a ->{STM} Boolean
IO.concurrent.STM.TMap.delete : Bytes -> TMap a ->{STM} ()
IO.concurrent.STM.TMap.empty : '{STM} TMap a
type IO.concurrent.STM.TMap.impl.F a
IO.concurrent.STM.TMap.impl.F.Empty : F a
IO.concurrent.STM.TMap.impl.F.Many : TMap a -> F a
IO.concurrent.STM.TMap.impl.F.One : Bytes -> a -> F a
IO.concurrent.STM.TMap.insert : Bytes -> a -> TMap a ->{STM} ()
IO.concurrent.STM.TMap.insert.impl : Nat -> Bytes -> a -> TMap a ->{STM} ()
IO.concurrent.STM.TMap.lookup : Bytes -> TMap a ->{STM} Optional a
IO.concurrent.STM.TMap.TMap : TVar (Optional a) -> [TVar (F a)] -> TMap a
type IO.concurrent.STM.TQueue a
IO.concurrent.STM.TQueue.boundedEnqueue : Nat -> a -> TQueue a ->{STM} ()
IO.concurrent.STM.TQueue.dequeue : TQueue a ->{STM} a
IO.concurrent.STM.TQueue.dequeueNonce : TQueue a ->{STM} Nat
IO.concurrent.STM.TQueue.elements : TQueue a ->{STM} [a]
IO.concurrent.STM.TQueue.empty : '{STM} TQueue a
IO.concurrent.STM.TQueue.enqueue : a -> TQueue a ->{STM} ()
IO.concurrent.STM.TQueue.enqueueAll : [a] -> TQueue a ->{STM} ()
IO.concurrent.STM.TQueue.fromList : [a] ->{STM} TQueue a
IO.concurrent.STM.TQueue.peek : TQueue a ->{STM} a
IO.concurrent.STM.TQueue.pushback : a -> TQueue a ->{STM} ()
IO.concurrent.STM.TQueue.size : TQueue a ->{STM} Nat
IO.concurrent.STM.TQueue.TQueue : TVar [a] -> TVar Nat -> TQueue a
IO.concurrent.STM.TQueue.tryBoundedEnqueue : Nat -> a -> TQueue a ->{STM} Boolean
IO.concurrent.STM.TQueue.tryBoundedEnqueueAll : Nat -> [a] -> TQueue a ->{STM} Boolean
IO.concurrent.STM.TQueue.tryDequeue : TQueue a ->{STM} Optional a
IO.concurrent.STM.TQueue.tryPeek : TQueue a ->{STM} Optional a
builtin type IO.concurrent.ThreadId
IO.concurrent.ThreadId.toText : ThreadId -> Text
type IO.concurrent.ThreadKilledFailure
structural type IO.concurrent.TMVar a
IO.concurrent.TMVar.isEmpty : TMVar a ->{STM} Boolean
IO.concurrent.TMVar.new : a ->{STM} TMVar a
IO.concurrent.TMVar.newEmpty : '{STM} TMVar a
IO.concurrent.TMVar.newEmptyIO : '{IO} TMVar a
IO.concurrent.TMVar.newIO : a ->{IO} TMVar a
IO.concurrent.TMVar.put : TMVar a -> a ->{STM} ()
IO.concurrent.TMVar.read : TMVar a ->{STM} a
IO.concurrent.TMVar.swap : TMVar a -> a ->{STM} a
IO.concurrent.TMVar.take : TMVar a ->{STM} a
IO.concurrent.TMVar.TMVar : TVar (Optional a) -> TMVar a
IO.concurrent.TMVar.tryPut : TMVar a -> a ->{STM} Boolean
IO.concurrent.TMVar.tryRead : TMVar a ->{STM} Optional a
IO.concurrent.TMVar.tryTake : TMVar a ->{STM} Optional a
builtin type IO.concurrent.TVar
IO.concurrent.TVar.modify : TVar a -> (a ->{STM} a) ->{STM} ()
IO.concurrent.TVar.new : a ->{STM} TVar a
IO.concurrent.TVar.newIO : a ->{IO} TVar a
IO.concurrent.TVar.read : TVar a ->{STM} a
IO.concurrent.TVar.readIO : TVar a ->{IO} a
IO.concurrent.TVar.state : TVar s -> (s ->{e} (a, s)) ->{e, STM} a
IO.concurrent.TVar.swap : TVar a -> a ->{STM} a
IO.concurrent.TVar.write : TVar a -> a ->{STM} ()
IO.console.printLine : Text ->{IO, Exception} ()
IO.console.readLine : '{IO, Exception} Text
type IO.deprecated.EpochTime
IO.deprecated.EpochTime.EpochTime : Nat -> EpochTime
IO.deprecated.systemTime : '{IO, Exception} EpochTime
IO.deprecated.systemTime.impl : '{IO} Either Failure Nat
IO.deprecated.systemTimeMicroseconds : '{IO} Int
IO.deprecated.systemTimeMicroseconds.impl : '{IO} Int
type IO.Failure
type IO.Failure.ArithmeticFailure
IO.Failure.Failure : Type -> Text -> Any -> Failure
IO.Failure.failureType : Failure -> Type
IO.Failure.message : Failure -> Text
type IO.Failure.MiscFailure
IO.Failure.payload : Failure -> Any
type IO.Failure.RuntimeFailure
type IO.FilePath
IO.FilePath./ : FilePath -> Text -> FilePath
IO.FilePath.appendFile : FilePath -> Bytes ->{IO, Exception} ()
IO.FilePath.appendFileUtf8 : FilePath -> Text ->{IO, Exception} ()
IO.FilePath.createDirectory : FilePath ->{IO, Exception} ()
IO.FilePath.createDirectory.deprecated : Text ->{IO, Exception} ()
IO.FilePath.createDirectory.impl : Text ->{IO} Either Failure ()
IO.FilePath.createTempDirectory : FilePath ->{IO, Exception} FilePath
IO.FilePath.createTempDirectory.impl : Text ->{IO} Either Failure Text
IO.FilePath.directoryContents : FilePath ->{IO, Exception} [FilePath]
IO.FilePath.directoryContents.impl : Text ->{IO} Either Failure [Text]
IO.FilePath.directoryContents.texts : FilePath ->{IO, Exception} [Text]
IO.FilePath.exists : FilePath ->{IO, Exception} Boolean
IO.FilePath.exists.deprecated : Text ->{IO, Exception} Boolean
IO.FilePath.exists.impl : Text ->{IO} Either Failure Boolean
type IO.FilePath.FileMode
IO.FilePath.FileMode.Append : FileMode
IO.FilePath.FileMode.Read : FileMode
IO.FilePath.FileMode.ReadWrite : FileMode
IO.FilePath.FileMode.Write : FileMode
IO.FilePath.FilePath : Text -> FilePath
IO.FilePath.getCurrentDirectory : '{IO, Exception} FilePath
IO.FilePath.getCurrentDirectory.deprecated : '{IO, Exception} Text
IO.FilePath.getCurrentDirectory.impl : '{IO} Either Failure Text
IO.FilePath.getSize : FilePath ->{IO, Exception} Nat
IO.FilePath.getSize.deprecated : Text ->{IO, Exception} Nat
IO.FilePath.getSize.impl : Text ->{IO} Either Failure Nat
IO.FilePath.getTempDirectory : '{IO, Exception} FilePath
IO.FilePath.getTempDirectory.deprecated : '{IO, Exception} Text
IO.FilePath.getTempDirectory.impl : '{IO} Either Failure Text
IO.FilePath.getTimestamp : FilePath ->{IO, Exception} Instant
IO.FilePath.getTimestamp.deprecated.v1 : Text ->{IO, Exception} Nat
IO.FilePath.getTimestamp.deprecated.v2 : FilePath ->{IO, Exception} EpochTime
IO.FilePath.getTimestamp.impl : Text ->{IO} Either Failure Nat
IO.FilePath.isDirectory : FilePath ->{IO, Exception} Boolean
IO.FilePath.isDirectory.deprecated : Text ->{IO, Exception} Boolean
IO.FilePath.isDirectory.impl : Text ->{IO} Either Failure Boolean
IO.FilePath.open : FilePath -> FileMode ->{IO, Exception} Handle
IO.FilePath.open.deprecated : Text -> FileMode ->{IO, Exception} Handle
IO.FilePath.open.impl : Text -> FileMode ->{IO} Either Failure Handle
IO.FilePath.readFile : FilePath ->{IO, Exception} Bytes
IO.FilePath.readFileUtf8 : FilePath ->{IO, Exception} Text
IO.FilePath.removeDirectory : FilePath ->{IO, Exception} ()
IO.FilePath.removeDirectory.deprecated : Text ->{IO, Exception} ()
IO.FilePath.removeDirectory.impl : Text ->{IO} Either Failure ()
IO.FilePath.removeFile : FilePath ->{IO, Exception} ()
IO.FilePath.removeFile.deprecated : Text ->{IO, Exception} ()
IO.FilePath.removeFile.impl : Text ->{IO} Either Failure ()
IO.FilePath.renameDirectory : FilePath -> FilePath ->{IO, Exception} ()
IO.FilePath.renameDirectory.deprecated : Text -> Text ->{IO, Exception} ()
IO.FilePath.renameDirectory.impl : Text -> Text ->{IO} Either Failure ()
IO.FilePath.renameFile : FilePath -> FilePath ->{IO, Exception} ()
IO.FilePath.renameFile.deprecated : Text -> Text ->{IO, Exception} ()
IO.FilePath.renameFile.impl : Text -> Text ->{IO} Either Failure ()
IO.FilePath.setCurrentDirectory : FilePath ->{IO, Exception} ()
IO.FilePath.setCurrentDirectory.deprecated : Text ->{IO, Exception} ()
IO.FilePath.setCurrentDirectory.impl : Text ->{IO} Either Failure ()
IO.FilePath.toText : FilePath -> Text
IO.FilePath.writeFile : FilePath -> Bytes ->{IO, Exception} ()
IO.FilePath.writeFileUtf8 : FilePath -> Text ->{IO, Exception} ()
IO.getArgs : '{IO, Exception} [Text]
IO.getArgs.impl : '{IO} Either Failure [Text]
IO.getEnv : Text ->{IO, Exception} Text
IO.getEnv.impl : Text ->{IO} Either Failure Text
builtin type IO.Handle
type IO.Handle.BufferMode
IO.Handle.BufferMode.BlockBuffering : BufferMode
IO.Handle.BufferMode.LineBuffering : BufferMode
IO.Handle.BufferMode.NoBuffering : BufferMode
IO.Handle.BufferMode.SizedBlockBuffering : Nat -> BufferMode
IO.Handle.close : Handle ->{IO, Exception} ()
IO.Handle.close.impl : Handle ->{IO} Either Failure ()
IO.Handle.getAllBytes : Handle ->{IO, Exception} Bytes
IO.Handle.getAllText : Handle ->{IO, Exception} Text
IO.Handle.getBuffering : Handle ->{IO, Exception} BufferMode
IO.Handle.getBuffering.impl : Handle ->{IO} Either Failure BufferMode
IO.Handle.getBytes : Handle -> Nat ->{IO, Exception} Bytes
IO.Handle.getBytes.impl : Handle -> Nat ->{IO} Either Failure Bytes
IO.Handle.getChar : Handle ->{IO, Exception} Char
IO.Handle.getChar.impl : Handle ->{IO} Either Failure Char
IO.Handle.getContents : Handle ->{IO, Exception} Bytes
IO.Handle.getEcho : Handle ->{IO, Exception} Boolean
IO.Handle.getEcho.impl : Handle ->{IO} Either Failure Boolean
IO.Handle.getLine : Handle ->{IO, Exception} Text
IO.Handle.getLine.impl : Handle ->{IO} Either Failure Text
IO.Handle.getPosition : Handle ->{IO, Exception} Nat
IO.Handle.getPosition.impl : Handle ->{IO} Either Failure Nat
IO.Handle.getSomeBytes : Handle -> Nat ->{IO, Exception} Bytes
IO.Handle.getSomeBytes.impl : Handle -> Nat ->{IO} Either Failure Bytes
IO.Handle.getText : Handle ->{IO, Exception} Text
IO.Handle.isEOF : Handle ->{IO, Exception} Boolean
IO.Handle.isEOF.impl : Handle ->{IO} Either Failure Boolean
IO.Handle.isOpen : Handle ->{IO, Exception} Boolean
IO.Handle.isOpen.impl : Handle ->{IO} Either Failure Boolean
IO.Handle.isReadable : Handle ->{IO} Boolean
IO.Handle.isSeekable : Handle ->{IO, Exception} Boolean
IO.Handle.isSeekable.impl : Handle ->{IO} Either Failure Boolean
IO.Handle.position : Handle ->{IO, Exception} Nat
IO.Handle.putBytes : Handle -> Bytes ->{IO, Exception} ()
IO.Handle.putBytes.impl : Handle -> Bytes ->{IO} Either Failure ()
IO.Handle.putLine : Handle -> Text ->{IO, Exception} ()
IO.Handle.putText : Handle -> Text ->{IO, Exception} ()
IO.Handle.ready : Handle ->{IO, Exception} Boolean
IO.Handle.ready.impl : Handle ->{IO} Either Failure Boolean
IO.Handle.readyAndAble : Handle ->{IO} Boolean
IO.Handle.seek : Handle -> SeekMode -> Int ->{IO, Exception} ()
IO.Handle.seek.impl : Handle -> SeekMode -> Int ->{IO} Either Failure ()
type IO.Handle.SeekMode
IO.Handle.SeekMode.AbsoluteSeek : SeekMode
IO.Handle.SeekMode.RelativeSeek : SeekMode
IO.Handle.SeekMode.SeekFromEnd : SeekMode
IO.Handle.setBuffering : Handle -> BufferMode ->{IO, Exception} ()
IO.Handle.setBuffering.impl : Handle -> BufferMode ->{IO} Either Failure ()
IO.Handle.setEcho : Handle -> Boolean ->{IO, Exception} ()
IO.Handle.setEcho.impl : Handle -> Boolean ->{IO} Either Failure ()
type IO.Handle.Std
IO.Handle.std : Std -> Handle
IO.Handle.Std.StdErr : Std
IO.Handle.Std.StdIn : Std
IO.Handle.Std.StdOut : Std
IO.Handle.stdErr : Handle
IO.Handle.stdIn : Handle
IO.Handle.stdOut : Handle
IO.Handle.toText : Handle -> Text
type IO.IOFailure
type IO.net.Connection
IO.net.Connection.accept : ListeningServerSocket ->{IO, Exception} Connection
IO.net.Connection.client : HostName -> Port ->{IO, Exception} Connection
IO.net.Connection.close : Connection ->{IO, Exception} ()
IO.net.Connection.closer : Connection -> '{IO, Exception} ()
IO.net.Connection.closer.modify : ('{IO, Exception} () ->{g} '{IO, Exception} ())
                                  -> Connection
                                  ->{g} Connection
IO.net.Connection.closer.set : '{IO, Exception} () -> Connection -> Connection
IO.net.Connection.Connection : (Bytes ->{IO, Exception} ())
                               -> '{IO, Exception} Bytes
                               -> '{IO, Exception} ()
                               -> Connection
IO.net.Connection.receive : Connection ->{IO, Exception} Bytes
IO.net.Connection.receiver : Connection -> '{IO, Exception} Bytes
IO.net.Connection.receiver.modify : ('{IO, Exception} Bytes ->{g} '{IO, Exception} Bytes)
                                    -> Connection
                                    ->{g} Connection
IO.net.Connection.receiver.set : '{g, IO, Exception} Bytes -> Connection -> Connection
IO.net.Connection.send : Connection -> Bytes ->{IO, Exception} ()
IO.net.Connection.send.modify : ((Bytes ->{IO, Exception} ())
                                ->{g} Bytes
                                ->{IO, Exception} ())
                                -> Connection
                                ->{g} Connection
IO.net.Connection.send.set : (Bytes ->{IO, Exception} ()) -> Connection -> Connection
IO.net.Connection.sizedSocket : Nat -> Socket ->{IO, Exception} Connection
IO.net.Connection.socket : Socket -> Connection
IO.net.Connection.tls : HostName -> Port ->{IO, Exception} Connection
IO.net.Connection.tls.deprecated : TlsSocket -> Connection
IO.net.Connection.tls.fromSocket : Socket -> HostName ->{IO, Exception} Connection
IO.net.Connection.tls.fromSocketWithConfig : ClientConfig
                                             -> Socket
                                             ->{IO, Exception} Connection
IO.net.Connection.tls.withConfig : HostName
                                   -> Port
                                   -> ClientConfig
                                   ->{IO, Exception} Connection
type IO.net.HostName
IO.net.HostName.HostName : Text -> HostName
IO.net.HostName.toText : HostName -> Text
type IO.net.Port
IO.net.Port.number : Nat -> Port
IO.net.Port.Port : Text -> Port
IO.net.Port.toText : Port -> Text
builtin type IO.net.Socket
IO.net.Socket.accept : ListeningServerSocket ->{IO, Exception} Socket
IO.net.Socket.accept.impl : Socket ->{IO} Either Failure Socket
IO.net.Socket.accept.raw : Socket ->{IO, Exception} Socket
type IO.net.Socket.BoundServerSocket
IO.net.Socket.BoundServerSocket.BoundServerSocket : Socket -> BoundServerSocket
IO.net.Socket.client : HostName -> Port ->{IO, Exception} Socket
IO.net.Socket.client.deprecated : Text -> Text ->{IO, Exception} Socket
IO.net.Socket.client.impl : Text -> Text ->{IO} Either Failure Socket
IO.net.Socket.close : Socket ->{IO, Exception} ()
IO.net.Socket.close.impl : Socket ->{IO} Either Failure ()
IO.net.Socket.listen : BoundServerSocket ->{IO, Exception} ListeningServerSocket
IO.net.Socket.listen.impl : Socket ->{IO} Either Failure ()
IO.net.Socket.listen.raw : Socket ->{IO, Exception} ()
type IO.net.Socket.ListeningServerSocket
IO.net.Socket.ListeningServerSocket.ListeningServerSocket : Socket -> ListeningServerSocket
IO.net.Socket.port : Socket ->{IO, Exception} Port
IO.net.Socket.port.impl : Socket ->{IO} Either Failure Nat
IO.net.Socket.portNumber : Socket ->{IO, Exception} Nat
IO.net.Socket.receive : Socket ->{IO, Exception} Bytes
IO.net.Socket.receiveAtMost : Socket -> Nat ->{IO, Exception} Bytes
IO.net.Socket.receiveAtMost.impl : Socket -> Nat ->{IO} Either Failure Bytes
IO.net.Socket.send : Socket -> Bytes ->{IO, Exception} ()
IO.net.Socket.send.impl : Socket -> Bytes ->{IO} Either Failure ()
IO.net.Socket.server : Optional HostName -> Port ->{IO, Exception} BoundServerSocket
IO.net.Socket.server.deprecated : Optional Text -> Text ->{IO, Exception} Socket
IO.net.Socket.server.impl : Optional Text -> Text ->{IO} Either Failure Socket
IO.net.Socket.server.raw : Optional HostName -> Port ->{IO, Exception} Socket
IO.net.Socket.toText : Socket -> Text
type IO.net.Socket.UnboundServerSocket
IO.net.Socket.UnboundServerSocket.UnboundServerSocket : Socket -> UnboundServerSocket
builtin type IO.net.Tls
builtin type IO.net.Tls.Cipher
builtin type IO.net.Tls.ClientConfig
IO.net.Tls.ClientConfig.certificates.set : [SignedCert] -> ClientConfig -> ClientConfig
IO.net.Tls.ClientConfig.default : HostName -> Text -> ClientConfig
IO.net.Tls.ClientConfig.default.impl : Text -> Bytes -> ClientConfig
IO.net.Tls.decodeCert : Bytes -> Either Failure SignedCert
IO.net.Tls.decodeCert.impl : Bytes -> Either Failure SignedCert
IO.net.Tls.decodePrivateKey : Bytes -> [Tls.PrivateKey]
IO.net.Tls.encodeCert : SignedCert -> Bytes
IO.net.Tls.encodePrivateKey : Tls.PrivateKey -> Bytes
IO.net.Tls.handshake : Tls ->{IO, Exception} TlsSocket
IO.net.Tls.handshake.impl : Tls ->{IO} Either Failure ()
IO.net.Tls.newClient : ClientConfig -> Socket ->{IO, Exception} Tls
IO.net.Tls.newClient.impl : ClientConfig -> Socket ->{IO} Either Failure Tls
IO.net.Tls.newServer : ServerConfig -> Socket ->{IO, Exception} Tls
IO.net.Tls.newServer.impl : ServerConfig -> Socket ->{IO} Either Failure Tls
builtin type IO.net.Tls.PrivateKey
builtin type IO.net.Tls.ServerConfig
IO.net.Tls.ServerConfig.certificates.set : [SignedCert] -> ServerConfig -> ServerConfig
IO.net.Tls.ServerConfig.ciphers.set : [Cipher] -> ServerConfig -> ServerConfig
IO.net.Tls.ServerConfig.default : [SignedCert] -> Tls.PrivateKey -> ServerConfig
IO.net.Tls.ServerConfig.versions.set : [Version] -> ServerConfig -> ServerConfig
builtin type IO.net.Tls.SignedCert
IO.net.Tls.terminate : Tls ->{IO, Exception} ()
IO.net.Tls.terminate.impl : Tls ->{IO} Either Failure ()
type IO.net.Tls.TlsFailure
type IO.net.Tls.TlsSocket
IO.net.Tls.TlsSocket.receive : TlsSocket ->{IO, Exception} Bytes
IO.net.Tls.TlsSocket.receive.impl : Tls ->{IO} Either Failure Bytes
IO.net.Tls.TlsSocket.send : TlsSocket -> Bytes ->{IO, Exception} ()
IO.net.Tls.TlsSocket.send.impl : Tls -> Bytes ->{IO} Either Failure ()
IO.net.Tls.TlsSocket.terminate : TlsSocket ->{IO, Exception} ()
IO.net.Tls.TlsSocket.terminate_ : TlsSocket ->{IO} ()
IO.net.Tls.TlsSocket.TlsSocket : Tls -> TlsSocket
builtin type IO.net.Tls.Version
type IO.net.URI
type IO.net.URI.Authority
IO.net.URI.authority : URI -> Optional Authority
IO.net.URI.Authority.Authority : Optional UserInfo
                                 -> HostName
                                 -> Optional Port
                                 -> Authority
IO.net.URI.Authority.fromHost : HostName -> Authority
IO.net.URI.Authority.host : Authority -> HostName
IO.net.URI.Authority.host.modify : (HostName ->{g} HostName) -> Authority ->{g} Authority
IO.net.URI.Authority.host.set : HostName -> Authority -> Authority
IO.net.URI.authority.modify : (Optional Authority ->{g} Optional Authority)
                              -> URI
                              ->{g} URI
IO.net.URI.Authority.port : Authority -> Optional Port
IO.net.URI.Authority.port.modify : (Optional Port ->{g} Optional Port)
                                   -> Authority
                                   ->{g} Authority
IO.net.URI.Authority.port.set : Optional Port -> Authority -> Authority
IO.net.URI.authority.set : Optional Authority -> URI -> URI
IO.net.URI.Authority.toText : Authority -> Text
IO.net.URI.Authority.userInfo : Authority -> Optional UserInfo
IO.net.URI.Authority.userInfo.modify : (Optional UserInfo ->{g} Optional UserInfo)
                                       -> Authority
                                       ->{g} Authority
IO.net.URI.Authority.userInfo.set : Optional UserInfo -> Authority -> Authority
IO.net.URI.encode.percent.char.special : Char -> Text
IO.net.URI.escape : Text -> Text
IO.net.URI.escapeChar : Char -> Text
IO.net.URI.escapeQuery : Text -> Text
IO.net.URI.escapeQueryChar : Char -> Text
IO.net.URI.forceHostAndPort : URI -> Authority
type IO.net.URI.Fragment
IO.net.URI.fragment : URI -> Fragment
IO.net.URI.Fragment.empty : Fragment
IO.net.URI.Fragment.Fragment : Text -> Fragment
IO.net.URI.Fragment.toText : Fragment -> Text
IO.net.URI.fragmentText : URI -> Text
IO.net.URI.fromPath : Path -> URI
IO.net.URI.host : URI -> HostName
IO.net.URI.http : URI -> URI
IO.net.URI.https : URI -> URI
type IO.net.URI.Method
IO.net.URI.Method.CONNECT : Method
IO.net.URI.Method.DELETE : Method
IO.net.URI.Method.GET : Method
IO.net.URI.Method.HEAD : Method
IO.net.URI.Method.OPTIONS : Method
IO.net.URI.Method.PATCH : Method
IO.net.URI.Method.POST : Method
IO.net.URI.Method.PUT : Method
IO.net.URI.Method.TRACE : Method
IO.net.URI.parse : Text ->{Exception} URI
IO.net.URI.parse.unquotePercentEncoded : Text ->{Exception} Text
IO.net.URI.parse.unquotePercentEncoded.impl : Text -> Either Text Text
IO.net.URI.parse.unquotePercentEncodedPlus : Text ->{Exception} Text
IO.net.URI.parse.unquotePercentEncodedPlus.impl : Text -> Either Text Text
IO.net.URI.parse._internal.parseAuthority : Text
                                            -> Text
                                            -> Text
                                            ->{Abort} Optional Authority
IO.net.URI.parse._internal.parseFragment : Text ->{Abort} Fragment
IO.net.URI.parse._internal.parsePath : Text ->{Abort} Path
IO.net.URI.parse._internal.parsePort : Text ->{Abort} Optional Port
IO.net.URI.parse._internal.parseQuery : Text ->{Abort} Query
IO.net.URI.parse._internal.parseUserInfo : Text ->{Abort} Optional UserInfo
IO.net.URI.parse._internal.renderPercentEncoded : Text ->{Abort} Text
type IO.net.URI.ParseError
IO.net.URI.parseOptional : Text -> Optional URI
IO.net.URI.parseOrBug : Text -> URI
type IO.net.URI.Path
IO.net.URI.path : URI -> Path
IO.net.URI.Path.++ : Path -> Path -> Path
IO.net.URI.Path./ : Path -> Text -> Path
IO.net.URI.Path.encode : Path -> Bytes
IO.net.URI.Path.encode.char : Char -> Text
IO.net.URI.Path.encode.segment : Text -> Bytes
IO.net.URI.Path.fromText : Text ->{Exception} Path
IO.net.URI.path.modify : (Path ->{g} Path) -> URI ->{g} URI
IO.net.URI.Path.Path : [Text] -> Path
IO.net.URI.Path.root : Path
IO.net.URI.Path.segments : Path -> [Text]
IO.net.URI.Path.segments.modify : ([Text] ->{g} [Text]) -> Path ->{g} Path
IO.net.URI.Path.segments.set : [Text] -> Path -> Path
IO.net.URI.path.set : Path -> URI -> URI
IO.net.URI.Path.toText : Path -> Text
IO.net.URI.Path.toUnescapedText : Path -> Text
IO.net.URI.pattern.alphaNum : Pattern Text
IO.net.URI.pattern.authority : IPattern (And (And Capture Capture) Capture) Text
IO.net.URI.pattern.dash : Pattern Text
IO.net.URI.pattern.decOctet : Pattern Text
IO.net.URI.pattern.fragment : IPattern Capture Text
IO.net.URI.pattern.h16 : Pattern Text
IO.net.URI.pattern.heirPart : IPattern
                                (And (And (And Capture Capture) Capture) Capture) Text
IO.net.URI.pattern.host : IPattern Capture Text
IO.net.URI.pattern.ipLiteral : Pattern Text
IO.net.URI.pattern.ipv4Address : Pattern Text
IO.net.URI.pattern.ipv6Address : Pattern Text
IO.net.URI.pattern.ipvFuture : Pattern Text
IO.net.URI.pattern.ls32 : Pattern Text
IO.net.URI.pattern.path : Pattern Text
IO.net.URI.pattern.pathAbEmpty : Pattern Text
IO.net.URI.pattern.pathAbsolute : Pattern Text
IO.net.URI.pattern.pathEmpty : Pattern Text
IO.net.URI.pattern.pathNoscheme : Pattern Text
IO.net.URI.pattern.pathRootless : Pattern Text
IO.net.URI.pattern.pchar : Pattern Text
IO.net.URI.pattern.percentEncoded : Pattern Text
IO.net.URI.pattern.period : Pattern Text
IO.net.URI.pattern.port : IPattern Capture Text
IO.net.URI.pattern.query : IPattern Capture Text
IO.net.URI.pattern.regName : Pattern Text
IO.net.URI.pattern.scheme : IPattern Capture Text
IO.net.URI.pattern.segment : Pattern Text
IO.net.URI.pattern.segmentNz : Pattern Text
IO.net.URI.pattern.segmentNzNc : Pattern Text
IO.net.URI.pattern.subDelims : Pattern Text
IO.net.URI.pattern.tilde : Pattern Text
IO.net.URI.pattern.underscore : Pattern Text
IO.net.URI.pattern.unreserved : Pattern Text
IO.net.URI.pattern.uri : IPattern
                           (And
                             (And
                               (And
                                 Capture (And (And (And Capture Capture) Capture) Capture))
                               Capture)
                             Capture)
                           Text
IO.net.URI.pattern.userInfo : IPattern Capture Text
IO.net.URI.port : URI -> Optional Port
IO.net.URI.portOr80 : URI -> Port
type IO.net.URI.Query
IO.net.URI.query : URI -> RawQuery
IO.net.URI.Query.& : Query -> (Text, Text) -> Query
IO.net.URI.Query.addParam : Text -> Text -> Query -> Query
IO.net.URI.Query.empty : Query
IO.net.URI.Query.encode.char : Text -> Text
IO.net.URI.Query.encode.charX : Char -> Text
IO.net.URI.Query.encode.impl : Boolean -> Query -> Text
IO.net.URI.Query.example.exampleQuery : Query
IO.net.URI.Query.fromRawQuery : RawQuery -> Optional Query
IO.net.URI.Query.getParam : Text -> Query -> [Text]
IO.net.URI.Query.Query : Map Text [Text] -> Query
IO.net.URI.Query.singleton : Text -> Text -> Query
IO.net.URI.Query.toMap : Query -> Map Text [Text]
IO.net.URI.Query.toRawQuery : Query -> RawQuery
IO.net.URI.Query.toText : Query -> Text
IO.net.URI.Query.union : Query -> Query -> Query
type IO.net.URI.RawQuery
IO.net.URI.RawQuery.empty : RawQuery
IO.net.URI.RawQuery.encode : RawQuery -> Text
IO.net.URI.RawQuery.encodePairs : [Char] -> Text
IO.net.URI.RawQuery.encodeRaw.char : Char -> Text
IO.net.URI.RawQuery.encodeRaw.text : Text -> Text
IO.net.URI.RawQuery.fromQuery : Query -> RawQuery
IO.net.URI.RawQuery.modify : (RawQuery ->{g} RawQuery) -> URI ->{g} URI
IO.net.URI.RawQuery.RawQuery : Text -> RawQuery
IO.net.URI.RawQuery.toQuery : RawQuery -> Optional Query
IO.net.URI.RawQuery.toText : RawQuery -> Text
type IO.net.URI.Scheme
IO.net.URI.scheme : URI -> Scheme
IO.net.URI.Scheme.defaultPort : Scheme ->{Throw Text} Nat
IO.net.URI.Scheme.empty : Scheme
IO.net.URI.Scheme.http : Scheme
IO.net.URI.Scheme.https : Scheme
IO.net.URI.scheme.modify : (Scheme ->{g} Scheme) -> URI ->{g} URI
IO.net.URI.Scheme.Scheme : Text -> Scheme
IO.net.URI.scheme.set : Scheme -> URI -> URI
IO.net.URI.Scheme.toText : Scheme -> Text
IO.net.URI.toText : URI -> Text
IO.net.URI.toUnescapedText : URI -> Text
IO.net.URI.URI : Scheme -> Optional Authority -> Path -> RawQuery -> Fragment -> URI
type IO.net.URI.UserInfo
IO.net.URI.UserInfo.UserInfo : Text -> UserInfo
IO.net.URI.withHost : Text -> URI -> URI
IO.net.URI.withQuery : RawQuery -> URI -> URI
builtin type IO.Process
IO.Process.call : Text -> [Text] ->{IO} Nat
IO.Process.exitCode : Process ->{IO} Optional Nat
IO.Process.kill : Process ->{IO} ()
IO.Process.start : Text -> [Text] ->{IO} (Handle, Handle, Handle, Process)
IO.Process.wait : Process ->{IO} Nat
IO.randomBytes : Nat ->{IO} Bytes
IO.randomNat : '{IO} Nat
IO.Raw.array : Nat ->{IO} mutable.Array.Raw {IO} a
IO.Raw.arrayOf : a -> Nat ->{IO} mutable.Array.Raw {IO} a
IO.Raw.byteArray : Nat ->{IO} mutable.ByteArray.Raw {IO}
IO.Raw.byteArrayOf : Nat -> Nat ->{IO} mutable.ByteArray.Raw {IO}
IO.ref : a ->{IO} Ref {IO} a
IO.ref.atomically : Ref {IO} a -> (a -> (a, b)) ->{IO} b
IO.ref.cas : Ref {IO} a -> Ticket a -> a ->{IO} Boolean
IO.ref.cas.examples.getAndIncrement : Ref {IO} Nat ->{IO} Nat
IO.ref.cas.examples.getAndIncrement2 : Ref {IO} Nat ->{IO} Nat
IO.ref.cas.examples.printAndIncrement : Ref {IO} Nat ->{IO, Exception} ()
IO.ref.modify : Ref {IO} a -> (a -> a) ->{IO} ()
IO.ref.readForCas : Ref {IO} a ->{IO} Ticket a
builtin type IO.ref.Ticket
IO.ref.Ticket.read : Ticket a -> a
IO.thawArray : data.Array a ->{IO} mutable.Array {IO} a
IO.thawByteArray : data.ByteArray ->{IO} mutable.ByteArray {IO}
IO.tryEval : '{IO, Exception} a ->{IO, Exception} a
IO.tryEval.impl : '{IO} a ->{IO, Exception} a
type IPattern n a
IPattern.++ : IPattern n a -> IPattern m a -> IPattern (And n m) a
IPattern.+: : Pattern a -> IPattern n a -> IPattern n a
IPattern.:+ : IPattern n a -> Pattern a -> IPattern n a
IPattern.<|> : IPattern n a -> IPattern n a -> IPattern n a
type IPattern.And l r
type IPattern.Capture
IPattern.capture : Pattern a -> IPattern Capture a
IPattern.IPattern : Pattern a -> IPattern n a
IPattern.run : IPattern n a -> a -> Optional ([a], a)
LICENSE : License
LocalDate.ordering : LocalDate -> LocalDate -> Ordering
LocalDateTime.ordering : LocalDateTime -> LocalDateTime -> Ordering
LocalTime.ordering : LocalTime -> LocalTime -> Ordering
type math.ArithmeticException
math.ArithmeticException.dividedByZero : '{Exception} r
math.ArithmeticException.DividedByZero : ArithmeticException
math.ArithmeticException.negativeInfinity : '{Exception} r
math.ArithmeticException.NegativeInfinityNotAllowed : ArithmeticException
math.ArithmeticException.notANumber : '{Exception} r
math.ArithmeticException.NotANumber : ArithmeticException
math.ArithmeticException.overflow : '{Exception} r
math.ArithmeticException.Overflow : ArithmeticException
math.ArithmeticException.positiveInfinity : '{Exception} r
math.ArithmeticException.PositiveInfinityNotAllowed : ArithmeticException
math.ArithmeticException.underflow : '{Exception} r
math.ArithmeticException.Underflow : ArithmeticException
type math.Natural
math.Natural.* : Natural -> Natural -> Natural
math.Natural.+ : Natural -> Natural -> Natural
math.Natural.- : Natural -> Natural -> Natural
math.Natural./ : Natural -> Natural ->{Exception} Natural
math.Natural.< : Natural -> Natural -> Boolean
math.Natural.<= : Natural -> Natural -> Boolean
math.Natural.== : Natural -> Natural -> Boolean
math.Natural.> : Natural -> Natural -> Boolean
math.Natural.>= : Natural -> Natural -> Boolean
math.Natural.div : Natural -> Natural ->{Exception} Natural
math.Natural.div.aborting : Natural -> Natural ->{Abort} Natural
math.Natural.divMod : Natural -> Natural ->{Exception} (Natural, Natural)
math.Natural.divMod.aborting : Natural -> Natural ->{Abort} (Natural, Natural)
math.Natural.eq : Natural -> Natural -> Boolean
math.Natural.fromNat : Nat -> Natural
math.Natural.gt : Natural -> Natural -> Boolean
math.Natural.gteq : Natural -> Natural -> Boolean
math.Natural.internal.bitMask : Nat
math.Natural.internal.bitWidth : Nat
math.Natural.internal.digits : Natural -> List.Nonempty Nat
math.Natural.internal.divImpl : Natural -> Natural ->{Abort} (Natural, Natural)
math.Natural.internal.fromNats : [Nat] -> Natural
math.Natural.internal.mkNatural : [Nat] -> Natural
math.Natural.internal.Natural : List.Nonempty Nat -> Natural
math.Natural.internal.normalize : Natural -> Natural
math.Natural.internal.radix : Nat
math.Natural.isZero : Natural -> Boolean
math.Natural.lt : Natural -> Natural -> Boolean
math.Natural.lteq : Natural -> Natural -> Boolean
math.Natural.maybeDiv : Natural -> Natural -> Optional Natural
math.Natural.maybeDivMod : Natural -> Natural -> Optional (Natural, Natural)
math.Natural.maybeMod : Natural -> Natural -> Optional Natural
math.Natural.mod : Natural -> Natural ->{Exception} Natural
math.Natural.mod.aborting : Natural -> Natural ->{Abort} Natural
math.Natural.one : Natural
math.Natural.parse : Nat -> Text -> Optional Natural
math.Natural.parse.deprecated : Text -> Nat -> Optional Natural
math.Natural.parse! : Nat -> Text ->{Abort} Natural
math.Natural.parse!.deprecated : Text -> Nat ->{Abort} Natural
math.Natural.pow : Natural -> Nat -> Natural
math.Natural.shiftLeft : Natural -> Nat -> Natural
math.Natural.toDecimalText : Natural -> Text
math.Natural.toHex : Natural -> Text
math.Natural.toMaybeNat : Natural -> Optional Nat
math.Natural.toNat : Natural ->{Exception} Nat
math.Natural.toText : Nat -> Natural -> Optional Text
math.Natural.toText.deprecated : Natural -> Nat -> Optional Text
math.Natural.toText! : Nat -> Natural ->{Abort} Text
math.Natural.toText!.deprecated : Natural -> Nat ->{Abort} Text
math.Natural.zero : Natural
math.Natural.^ : Natural -> Nat -> Natural
type metadata.Author
type metadata.CopyrightHolder
metadata.CopyrightHolder.CopyrightHolder : GUID -> Text -> CopyrightHolder
type metadata.IsPropagated
metadata.isPropagated : IsPropagated
metadata.IsPropagated.IsPropagated : IsPropagated
type metadata.License
metadata.License.copyrightHolders : License -> [CopyrightHolder]
metadata.License.copyrightHolders.modify : ([CopyrightHolder] ->{g} [CopyrightHolder])
                                           -> License
                                           ->{g} License
metadata.License.copyrightHolders.set : [CopyrightHolder] -> License -> License
metadata.License.License : [CopyrightHolder] -> [Year] -> LicenseType -> License
metadata.License.licenseType : License -> LicenseType
metadata.License.licenseType.modify : (LicenseType ->{g} LicenseType)
                                      -> License
                                      ->{g} License
metadata.License.licenseType.set : LicenseType -> License -> License
metadata.License.years : License -> [Year]
metadata.License.years.modify : ([Year] ->{g} [Year]) -> License ->{g} License
metadata.License.years.set : [Year] -> License -> License
type metadata.LicenseType
metadata.licenseTypes.allRightsReserved : LicenseType
metadata.licenseTypes.bsd2 : LicenseType
metadata.licenseTypes.bsd3 : LicenseType
metadata.licenseTypes.cc0 : LicenseType
metadata.licenseTypes.mit : LicenseType
metadata.unassignedGuids.guid3 : GUID
metadata.unassignedGuids.guid4 : GUID
metadata.unassignedGuids.guid5 : GUID
metadata.unassignedGuids.guid6 : GUID
metadata.unassignedGuids.guid8 : GUID
type metadata.Year
metadata.Year.toText : Year -> Text
metadata.Year.Year : Nat -> Year
structural type mutable.Array g a
mutable.Array.copy : Nat
                     -> Nat
                     -> Nat
                     -> mutable.Array g a
                     -> mutable.Array g a
                     ->{g, Exception} ()
mutable.Array.drop! : Nat -> mutable.Array g a ->{Exception} mutable.Array g a
mutable.Array.dropEnd! : Nat -> mutable.Array g a ->{Exception} mutable.Array g a
mutable.Array.fill : Nat -> a ->{Scope s} mutable.Array {Scope s} a
mutable.Array.freeze : mutable.Array g a ->{g} data.Array a
mutable.Array.freeze! : mutable.Array g a ->{g} data.Array a
mutable.Array.MArr : Nat -> Nat -> mutable.Array.Raw g a -> mutable.Array g a
mutable.Array.mutable.Array.fromList : [a] ->{Scope s} mutable.Array {Scope s} a
mutable.Array.of : x -> Nat ->{Scope s} mutable.Array {Scope s} x
builtin type mutable.Array.Raw
mutable.Array.Raw.copyTo! : mutable.Array.Raw g a
                            -> Nat
                            -> mutable.Array.Raw g a
                            -> Nat
                            -> Nat
                            ->{g, Exception} ()
mutable.Array.Raw.freeze : mutable.Array.Raw g a -> Nat -> Nat ->{g} data.Array.Raw a
mutable.Array.Raw.freeze! : mutable.Array.Raw g a ->{g} data.Array.Raw a
mutable.Array.Raw.read : mutable.Array.Raw g a -> Nat ->{g, Exception} a
mutable.Array.Raw.size : mutable.Array.Raw g a -> Nat
mutable.Array.Raw.write : mutable.Array.Raw g a -> Nat -> a ->{g, Exception} ()
mutable.Array.read : mutable.Array g a -> Nat ->{g, Exception} a
mutable.Array.resize : Nat
                       -> a
                       -> mutable.Array {Scope s} a
                       ->{Exception, Scope s} mutable.Array {Scope s} a
mutable.Array.resize.impl : (∀ x. x -> Nat ->{g} mutable.Array g x)
                            -> Nat
                            -> a
                            -> mutable.Array g a
                            ->{g, Exception} mutable.Array g a
mutable.Array.resizeIO : Nat
                         -> a
                         -> mutable.Array IO a
                         ->{IO, Exception} mutable.Array IO a
mutable.Array.size : mutable.Array g a -> Nat
mutable.Array.slice! : Nat -> Nat -> mutable.Array g a ->{Exception} mutable.Array g a
mutable.Array.write : mutable.Array g a -> Nat -> a ->{g, Exception} ()
structural type mutable.ByteArray g
mutable.ByteArray.drop! : Nat -> mutable.ByteArray g ->{Exception} mutable.ByteArray g
mutable.ByteArray.dropEnd! : Nat -> mutable.ByteArray g ->{Exception} mutable.ByteArray g
mutable.ByteArray.freeze : mutable.ByteArray g ->{g} data.ByteArray
mutable.ByteArray.freeze! : mutable.ByteArray g ->{g} data.ByteArray
mutable.ByteArray.MBArr : Nat -> Nat -> mutable.ByteArray.Raw g -> mutable.ByteArray g
builtin type mutable.ByteArray.Raw
mutable.ByteArray.Raw.copyTo! : mutable.ByteArray.Raw g
                                -> Nat
                                -> mutable.ByteArray.Raw g
                                -> Nat
                                -> Nat
                                ->{g, Exception} ()
mutable.ByteArray.Raw.freeze : mutable.ByteArray.Raw g
                               -> Nat
                               -> Nat
                               ->{g} data.ByteArray.Raw
mutable.ByteArray.Raw.freeze! : mutable.ByteArray.Raw g ->{g} data.ByteArray.Raw
mutable.ByteArray.Raw.read16be : mutable.ByteArray.Raw g -> Nat ->{g, Exception} Nat
mutable.ByteArray.Raw.read24be : mutable.ByteArray.Raw g -> Nat ->{g, Exception} Nat
mutable.ByteArray.Raw.read32be : mutable.ByteArray.Raw g -> Nat ->{g, Exception} Nat
mutable.ByteArray.Raw.read40be : mutable.ByteArray.Raw g -> Nat ->{g, Exception} Nat
mutable.ByteArray.Raw.read64be : mutable.ByteArray.Raw g -> Nat ->{g, Exception} Nat
mutable.ByteArray.Raw.read8 : mutable.ByteArray.Raw g -> Nat ->{g, Exception} Nat
mutable.ByteArray.Raw.size : mutable.ByteArray.Raw g -> Nat
mutable.ByteArray.Raw.write16be : mutable.ByteArray.Raw g -> Nat -> Nat ->{g, Exception} ()
mutable.ByteArray.Raw.write32be : mutable.ByteArray.Raw g -> Nat -> Nat ->{g, Exception} ()
mutable.ByteArray.Raw.write64be : mutable.ByteArray.Raw g -> Nat -> Nat ->{g, Exception} ()
mutable.ByteArray.Raw.write8 : mutable.ByteArray.Raw g -> Nat -> Nat ->{g, Exception} ()
mutable.ByteArray.read8 : mutable.ByteArray g -> Nat ->{g, Exception} Nat
mutable.ByteArray.size : mutable.ByteArray g -> Nat
mutable.ByteArray.slice! : Nat
                           -> Nat
                           -> mutable.ByteArray g
                           ->{Exception} mutable.ByteArray g
mutable.ByteArray.write16be : mutable.ByteArray g -> Nat -> Nat ->{g, Exception} ()
mutable.ByteArray.write32be : mutable.ByteArray g -> Nat -> Nat ->{g, Exception} ()
mutable.ByteArray.write64be : mutable.ByteArray g -> Nat -> Nat ->{g, Exception} ()
mutable.ByteArray.write8 : mutable.ByteArray g -> Nat -> Nat ->{g, Exception} ()
builtin type mutable.Ref
mutable.Ref.modify.deprecated : Ref g a -> (a ->{h} a) ->{g, h} ()
mutable.Ref.read : Ref g a ->{g} a
mutable.Ref.write : Ref g a -> a ->{g} ()
builtin type mutable.Scope
mutable.Scope.arrayOf : a -> Nat ->{Scope s} mutable.Array {Scope s} a
mutable.Scope.byteArray : Nat ->{Scope s} mutable.ByteArray {Scope s}
mutable.Scope.byteArrayOf : Nat -> Nat ->{Scope s} mutable.ByteArray {Scope s}
mutable.Scope.Raw.array : Nat ->{Scope s} mutable.Array.Raw (Scope s) a
mutable.Scope.Raw.arrayOf : a -> Nat ->{Scope s} mutable.Array.Raw (Scope s) a
mutable.Scope.Raw.byteArray : Nat ->{Scope s} mutable.ByteArray.Raw (Scope s)
mutable.Scope.Raw.byteArrayOf : Nat -> Nat ->{Scope s} mutable.ByteArray.Raw (Scope s)
mutable.Scope.ref : a ->{Scope s} Ref {Scope s} a
mutable.Scope.ref.modify : Ref {Scope s} a -> (a ->{g} a) ->{g, Scope s} ()
mutable.Scope.run : (∀ s. '{g, Scope s} r) ->{g} r
mutable.Scope.thawArray : data.Array a ->{Scope s} mutable.Array {Scope s} a
mutable.Scope.thawByteArray : data.ByteArray ->{Scope s} mutable.ByteArray {Scope s}
builtin type Nat
Nat.!= : Nat -> Nat -> Boolean
Nat.% : Nat -> Nat -> Nat
Nat.* : Nat -> Nat -> Nat
Nat.+ : Nat -> Nat -> Nat
Nat.- : Nat -> Nat -> Nat
Nat./ : Nat -> Nat -> Nat
Nat.< : Nat -> Nat -> Boolean
Nat.<= : Nat -> Nat -> Boolean
Nat.== : Nat -> Nat -> Boolean
Nat.> : Nat -> Nat -> Boolean
Nat.>= : Nat -> Nat -> Boolean
Nat.and : Nat -> Nat -> Nat
Nat.bit : Nat -> Nat -> Nat
Nat.changeBit : Nat -> Boolean -> Nat -> Nat
Nat.choose : Nat -> Nat -> Nat
Nat.clamp : Nat -> Nat -> Nat -> Nat
Nat.clearBit : Nat -> Nat -> Nat
Nat.complement : Nat -> Nat
Nat.decrement : Nat -> Nat
Nat.deprecated.findLowestZero : (Nat ->{e} Int) -> Nat -> Nat ->{e} Nat
Nat.diff : Nat -> Nat -> Nat
Nat.div : Nat -> Nat -> Nat
Nat.dropBits : Nat -> Nat -> Nat
Nat.eq : Nat -> Nat -> Boolean
Nat.factorial : Nat -> Natural
Nat.flipBit : Nat -> Nat -> Nat
Nat.flipEndian : Nat -> Nat
Nat.fromBytesBigEndian : Bytes ->{Abort} Nat
Nat.fromBytesLittleEndian : Bytes ->{Abort} Nat
Nat.fromHex : Text -> Optional Nat
Nat.fromInt : Int -> Optional Nat
Nat.fromText : Text -> Optional Nat
Nat.fromTextOrFail : Text ->{Exception} Nat
Nat.gcd : Nat -> Nat ->{Abort} Nat
Nat.gt : Nat -> Nat -> Boolean
Nat.gteq : Nat -> Nat -> Boolean
Nat.increment : Nat -> Nat
Nat.inRange : Nat -> Nat -> Nat -> Boolean
Nat.isEven : Nat -> Boolean
Nat.isOdd : Nat -> Boolean
Nat.isPrefixOf : Nat -> Nat -> Boolean
Nat.isSetBit : Nat -> Nat -> Boolean
Nat.isSuffixOf : Nat -> Nat -> Boolean
Nat.lcm : Nat -> Nat ->{Abort} Nat
Nat.leadingOnes : Nat -> Nat
Nat.leadingZeros : Nat -> Nat
Nat.lsfr : Nat -> Nat
Nat.lt : Nat -> Nat -> Boolean
Nat.lteq : Nat -> Nat -> Boolean
Nat.max : Nat -> Nat -> Nat
Nat.maxNat : Nat
Nat.maybeMultiply : Nat -> Nat ->{Abort} Nat
Nat.min : Nat -> Nat -> Nat
Nat.mod : Nat -> Nat -> Nat
Nat.modExp : Nat -> Nat -> Nat -> Nat
Nat.msb : Nat -> Optional Nat
Nat.neq : Nat -> Nat -> Boolean
Nat.onesComplementSum : Nat -> Nat -> Nat -> Nat
Nat.or : Nat -> Nat -> Nat
Nat.popCount : Nat -> Nat
Nat.pow : Nat -> Nat -> Nat
Nat.product : [Nat] -> Nat
Nat.range : Nat -> Nat -> [Nat]
Nat.rangeClosed : Nat -> Nat -> [Nat]
Nat.reverseBits : Nat -> Nat
Nat.search : (Nat ->{e} Int) -> Nat -> Nat ->{e} Optional Nat
Nat.setBit : Nat -> Nat -> Nat
Nat.shiftLeft : Nat -> Nat -> Nat
Nat.shiftRight : Nat -> Nat -> Nat
Nat.subtractToInt : Nat -> Nat -> Int
Nat.subtractToInt.impl : Nat -> Nat -> Int
Nat.sum : [Nat] -> Nat
Nat.takeLeftBits : Nat -> Nat -> Nat
Nat.takeRightBits : Nat -> Nat -> Nat
Nat.toBytesBigEndian : Nat -> Bytes
Nat.toBytesLittleEndian : Nat -> Bytes
Nat.toFloat : Nat -> Float
Nat.toInt : Nat -> Int
Nat.toText : Nat -> Text
Nat.toTextBase : Nat -> Nat -> Optional Text
Nat.trailingZeros : Nat -> Nat
Nat.twosComplement : Nat -> Nat
Nat.xor : Nat -> Nat -> Nat
OffsetDateTime.ordering : OffsetDateTime -> OffsetDateTime -> Ordering
OffsetTime.ordering : OffsetTime -> OffsetTime -> Ordering
structural type Optional a
Optional.<*> : Optional (a ->{g} b) -> Optional a ->{g} Optional b
Optional.apply : Optional (a ->{g} b) -> Optional a ->{g} Optional b
Optional.compareBy : (a ->{g2} a ->{g1} Ordering)
                     -> Optional a
                     -> Optional a
                     ->{g2, g1} Ordering
Optional.contains : a -> Optional a -> Boolean
Optional.deprecated.mapOptional : (a ->{g} Optional b) -> [a] ->{g} [b]
Optional.exists : (a ->{g} Boolean) -> Optional a ->{g} Boolean
Optional.filter : (a ->{g} Boolean) -> Optional a ->{g} Optional a
Optional.flatMap : (a ->{g} Optional b) -> Optional a ->{g} Optional b
Optional.flatten : Optional (Optional a) -> Optional a
Optional.fold : '{g} b -> (a ->{g} b) -> Optional a ->{g} b
Optional.forAll : (a ->{g} Boolean) -> Optional a ->{g} Boolean
Optional.foreach : (a ->{g} ()) -> Optional a ->{g} ()
Optional.getOrBug : msg -> Optional a -> a
Optional.getOrElse : a -> Optional a -> a
Optional.getOrElse' : '{e} a -> Optional a ->{e} a
Optional.isNone : Optional a -> Boolean
Optional.isSome : Optional a -> Boolean
Optional.map : (a ->{g} b) -> Optional a ->{g} Optional b
Optional.map2 : (a ->{g} b ->{g} c) -> Optional a -> Optional b ->{g} Optional c
Optional.None : Optional a
Optional.note : e -> Optional a ->{Throw e} a
Optional.orElse : Optional a -> Optional a -> Optional a
Optional.Some : a -> Optional a
Optional.toAbort : Optional a ->{Abort} a
Optional.toException : Text -> Type -> Optional a ->{Exception} a
Optional.toGenericException : Optional a ->{Exception} a
Optional.toGenericExceptionWith : Text -> b -> Optional a ->{Exception} a
Optional.toList : Optional a -> [a]
Optional.unzip : Optional (a, b) -> (Optional a, Optional b)
Optional.zip : Optional a -> Optional b -> Optional (a, b)
type Ordering
Ordering.andThen : Ordering -> Ordering -> Ordering
Ordering.andThen.example.ex1 : Ordering
Ordering.eqBy : (a ->{f} b ->{g} Ordering) -> a -> b ->{f, g} Boolean
Ordering.Equal : Ordering
Ordering.Greater : Ordering
Ordering.gtBy : (a ->{g2} a ->{g1} Ordering) -> a -> a ->{g2, g1} Boolean
Ordering.gteqBy : (a ->{g2} a ->{g1} Ordering) -> a -> a ->{g2, g1} Boolean
Ordering.inverse : Ordering -> Ordering
Ordering.Less : Ordering
Ordering.list.orderingBy : (a -> a ->{g} Ordering) -> [a] -> [a] ->{g} Ordering
Ordering.ltBy : (a ->{g2} a ->{g1} Ordering) -> a -> a ->{g2, g1} Boolean
Ordering.lteqBy : (a ->{g2} a ->{g1} Ordering) -> a -> a ->{g2, g1} Boolean
Ordering.maxBy : (a ->{g2} a ->{g1} Ordering) -> a -> a ->{g2, g1} a
Ordering.medianOf3By : (a ->{g2} a ->{g1} Ordering) -> a -> a -> a ->{g2, g1} a
Ordering.minBy : (a ->{g2} a ->{g1} Ordering) -> a -> a ->{g2, g1} a
Ordering.pair.orderingBy : (a -> a ->{g2} Ordering)
                           -> (b -> b ->{g} Ordering)
                           -> (a, b)
                           -> (a, b)
                           ->{g, g2} Ordering
builtin type Pattern
Pattern.+ : Pattern a -> Pattern a -> Pattern a
Pattern.capture : Pattern a -> Pattern a
Pattern.captureAs : a -> Pattern a -> Pattern a
Pattern.captures : Pattern t -> t -> [t]
Pattern.drop : Pattern t -> t -> t
Pattern.empty : Pattern a
Pattern.isMatch : Pattern a -> a -> Boolean
Pattern.join : [Pattern a] -> Pattern a
Pattern.many : Pattern a -> Pattern a
Pattern.oneOf : List.Nonempty (Pattern a) -> Pattern a
Pattern.optional : Pattern a -> Pattern a
Pattern.or : Pattern a -> Pattern a -> Pattern a
Pattern.replicate : Nat -> Nat -> Pattern a -> Pattern a
Pattern.run : Pattern a -> a -> Optional ([a], a)
Pattern.sepMany : Pattern a -> Pattern a -> Pattern a
Pattern.sepSome : Pattern a -> Pattern a -> Pattern a
Pattern.some : Pattern a -> Pattern a
structural type Pretty txt
type Pretty.Annotated w txt
Pretty.Annotated.Append : w -> [Annotated w txt] -> Annotated w txt
Pretty.Annotated.Empty : Annotated w txt
Pretty.Annotated.Group : w -> Annotated w txt -> Annotated w txt
Pretty.Annotated.Indent : w
                          -> Annotated w txt
                          -> Annotated w txt
                          -> Annotated w txt
                          -> Annotated w txt
Pretty.Annotated.Lit : w -> txt -> Annotated w txt
Pretty.Annotated.OrElse : w -> Annotated w txt -> Annotated w txt -> Annotated w txt
Pretty.Annotated.Table : w -> [[Annotated w txt]] -> Annotated w txt
Pretty.Annotated.Wrap : w -> Annotated w txt -> Annotated w txt
Pretty.append : Pretty txt -> Pretty txt -> Pretty txt
Pretty.empty : Pretty txt
Pretty.get : Pretty txt -> Annotated () txt
Pretty.group : Pretty txt -> Pretty txt
Pretty.indent : Pretty txt -> Pretty txt -> Pretty txt
Pretty.indent' : Pretty txt -> Pretty txt -> Pretty txt -> Pretty txt
Pretty.join : [Pretty txt] -> Pretty txt
Pretty.lit : txt -> Pretty txt
Pretty.map : (txt ->{g} txt2) -> Pretty txt ->{g} Pretty txt2
Pretty.orElse : Pretty txt -> Pretty txt -> Pretty txt
Pretty.Pretty : Annotated () txt -> Pretty txt
Pretty.sepBy : Pretty txt -> [Pretty txt] -> Pretty txt
Pretty.table : [[Pretty txt]] -> Pretty txt
Pretty.wrap : Pretty txt -> Pretty txt
builtin type reflection.Code
reflection.Code.cache_ : [(Link.Term, Code)] ->{IO} [Link.Term]
reflection.Code.dependencies : Code -> [Link.Term]
reflection.Code.deserialize : Bytes ->{Exception} Code
reflection.Code.deserialize.impl : Bytes -> Either Text Code
reflection.Code.isMissing : Link.Term ->{IO} Boolean
reflection.Code.lookup : Link.Term ->{IO} Optional Code
reflection.Code.serialize : Code -> Bytes
reflection.Code.validateLinks : [(Link.Term, Code)]
                                ->{Exception} Either [Link.Term] [Link.Term]
type reflection.Link
builtin type reflection.Link.Term
reflection.Link.Term : Link.Term -> Link
reflection.Link.Term.toText : Link.Term -> Text
builtin type reflection.Link.Type
reflection.Link.Type : Type -> Link
type reflection.RewriteCase a b
reflection.RewriteCase.RewriteCase : a -> b -> RewriteCase a b
type reflection.Rewrites a
reflection.Rewrites.examples.eitherToOptional : e1
                                                -> a1
                                                -> Rewrites
                                                  ( RewriteTerm
                                                    (Either e1 b1) (Optional a5),
                                                    RewriteTerm
                                                    (Either a4 a1) (Optional a1),
                                                    RewriteCase (Either e1 b) (Optional a3),
                                                    RewriteCase
                                                    (Either a2 a1) (Optional a1),
                                                    RewriteSignature
                                                    (Either e a) (Optional a))
reflection.Rewrites.examples.etaReduce : (i ->{g} o)
                                         -> Rewrites
                                           (Tuple
                                             (RewriteTerm (i ->{g, g1} o) (i ->{g} o)) ())
reflection.Rewrites.examples.optionalToEither : ∀ _e a1 a5 b a4 a3 a2 a e.
                                                  _e
                                                  -> a1
                                                  -> Rewrites
                                                    ( RewriteTerm
                                                      (Optional a5) (Either _e b),
                                                      RewriteTerm
                                                      (Optional a1) (Either a4 a1),
                                                      RewriteCase
                                                      (Optional a1) (Either a3 a1),
                                                      RewriteCase
                                                      (Either a2 a1) (Optional a1),
                                                      RewriteSignature
                                                      (Optional a) (Either e a))
reflection.Rewrites.examples.useIncrement : Nat
                                            -> Rewrites (Tuple (RewriteTerm Nat Nat) ())
reflection.Rewrites.Rewrites : a -> Rewrites a
type reflection.RewriteSignature a b
reflection.RewriteSignature.RewriteSignature : (a -> b -> ()) -> RewriteSignature a b
type reflection.RewriteTerm a b
reflection.RewriteTerm.RewriteTerm : a -> b -> RewriteTerm a b
reflection.validateSandboxed.deprecated : [Link.Term] -> a -> Boolean
builtin type reflection.Value
reflection.Value.dependencies : Value -> [Link.Term]
reflection.Value.deserialize : Bytes ->{Exception} Value
reflection.Value.deserialize.impl : Bytes -> Either Text Value
reflection.Value.load : Value ->{IO} Either [Link.Term] a
reflection.Value.serialize : Value -> Bytes
reflection.Value.validateSandboxed : [Link.Term]
                                     -> Value
                                     ->{IO} Either [Link.Term] [Link.Term]
reflection.Value.validateSandboxed.example : '{IO} Either [Link.Term] [Link.Term]
reflection.Value.value : a -> Value
Stream.toArray : '{g, Stream b} () ->{g, Exception} data.Array b
type system.ANSI.Color
system.ANSI.Color.Black : Color
system.ANSI.Color.Blue : Color
system.ANSI.Color.BrightBlack : Color
system.ANSI.Color.BrightBlue : Color
system.ANSI.Color.BrightCyan : Color
system.ANSI.Color.BrightGreen : Color
system.ANSI.Color.BrightMagenta : Color
system.ANSI.Color.BrightRed : Color
system.ANSI.Color.BrightWhite : Color
system.ANSI.Color.BrightYellow : Color
system.ANSI.Color.Cyan : Color
system.ANSI.Color.Green : Color
system.ANSI.Color.Magenta : Color
system.ANSI.Color.Red : Color
system.ANSI.Color.White : Color
system.ANSI.Color.Yellow : Color
type system.ConsoleText
system.ConsoleText.Background : Color -> ConsoleText -> ConsoleText
system.ConsoleText.Bold : ConsoleText -> ConsoleText
system.ConsoleText.Foreground : Color -> ConsoleText -> ConsoleText
system.ConsoleText.Invert : ConsoleText -> ConsoleText
system.ConsoleText.Plain : Text -> ConsoleText
system.ConsoleText.Underline : ConsoleText -> ConsoleText
test.arbitrary.floats : Nat ->{Each, Random} Float
test.arbitrary.ints : Nat ->{Each, Random} Int
test.arbitrary.nats : Nat ->{Each, Random} Nat
test.arbitrary.unspecialFloats : Nat ->{Each, Random} Float
test.assert : Boolean -> e -> a -> a
test.assertEquals : a -> a -> Boolean
structural type test.deprecated.Domain a
test.deprecated.Domain.boolean : Domain Boolean
test.deprecated.Domain.ints : Domain Int
test.deprecated.Domain.Large : Weighted a -> Domain a
test.deprecated.Domain.lift2 : (a -> b -> c) -> Domain a -> Domain b -> Domain c
test.deprecated.Domain.lists : Domain [()]
test.deprecated.Domain.listsOf : Domain a -> Domain [a]
test.deprecated.Domain.map : (a -> b) -> Domain a -> Domain b
test.deprecated.Domain.nats : Domain Nat
test.deprecated.Domain.pairs : Domain a -> Domain (a, a)
test.deprecated.Domain.sample : Nat -> Domain a -> [a]
test.deprecated.Domain.Small : [a] -> Domain a
test.deprecated.Domain.smallSize : Nat
test.deprecated.Domain.tuples : Domain a -> Domain b -> Domain (Tuple a b)
test.deprecated.Domain.weighted : Domain a -> Weighted a
structural ability test.deprecated.Gen
test.deprecated.Gen.append : '{Gen} a -> '{Gen} a -> '{Gen} a
test.deprecated.Gen.append.examples.ex1 : [Nat]
test.deprecated.Gen.append.examples.ex2 : [Nat]
test.deprecated.gen.atLeastOne : '{Gen} a -> '{Gen} List.Nonempty a
test.deprecated.gen.atLeastOneDistinct : '{Gen} a -> '{Gen} List.Nonempty a
test.deprecated.gen.boolean : '{Gen} Boolean
test.deprecated.gen.Char.alpha : '{Gen} Char
test.deprecated.gen.Char.alpha.sampled : [Char]
test.deprecated.gen.Char.ascii : '{Gen} Char
test.deprecated.gen.Char.ascii.sampled : [Char]
test.deprecated.gen.Char.asciiNonPrintable : '{Gen} Char
test.deprecated.gen.Char.asciiNonPrintable.sampled : [Char]
test.deprecated.gen.Char.asciiPrintable : '{Gen} Char
test.deprecated.gen.Char.asciiPrintable.sampled : [Char]
test.deprecated.gen.Char.digit : '{Gen} Char
test.deprecated.gen.Char.digit.sampled : [Char]
test.deprecated.gen.Char.hexDigit : '{Gen} Char
test.deprecated.gen.Char.hexDigit.sampled : [Char]
test.deprecated.gen.Char.lower : '{Gen} Char
test.deprecated.gen.Char.lower.sampled : [Char]
test.deprecated.gen.Char.upper : '{Gen} Char
test.deprecated.gen.Char.upper.sampled : [Char]
test.deprecated.Gen.cost : Nat -> '{Gen} a -> '{Gen} a
test.deprecated.gen.distinctListOf : '{Gen} a -> '{Gen} [a]
test.deprecated.gen.either : '{Gen} a -> '{Gen} b -> '{Gen} Either a b
test.deprecated.gen.empty : '{Gen} a
test.deprecated.gen.float : '{Gen} Float
test.deprecated.gen.functions.logic : '{Gen} (Boolean -> Boolean -> Boolean)
test.deprecated.gen.functions.someOrNone : (a ->{g} b)
                                           -> b
                                           -> '{Gen} (Optional a
                                           ->{g} Optional b)
test.deprecated.gen.functions.yesNo : '{Gen} (Boolean -> Boolean)
test.deprecated.gen.int : '{Gen} Int
test.deprecated.gen.listOf : '{Gen} a -> '{Gen} [a]
test.deprecated.gen.mapOf : '{Gen} k -> '{Gen} v -> '{Gen} Map k v
test.deprecated.gen.nat : '{Gen} Nat
test.deprecated.gen.natIn : Nat -> Nat -> '{Gen} Nat
test.deprecated.gen.natInOrder : '{Gen} Nat
test.deprecated.gen.nonzeroNat : '{Gen} Nat
test.deprecated.gen.normalFloat : '{Gen} Float
test.deprecated.gen.oneOf : [a] -> '{Gen} a
test.deprecated.gen.optional : '{Gen} a -> '{Gen} Optional a
test.deprecated.gen.pairOf : '{Gen} a -> '{Gen} b -> '{Gen} (a, b)
test.deprecated.gen.pick : ['{Gen} a] -> '{Gen} a
test.deprecated.gen.positiveInt : '{Gen} Int
test.deprecated.Gen.runGen : '{g, Gen} a ->{g, Abort} a
test.deprecated.Gen.sample : Weighted a ->{Gen} a
test.deprecated.gen.setOf : '{Gen} a -> '{Gen} Set a
test.deprecated.Gen.take : Nat -> '{e, Gen} t -> '{e, Gen} t
test.deprecated.gen.Text.ascii : '{Gen} Text
test.deprecated.gen.Text.asciiPrintable : '{Gen} Text
test.deprecated.Gen.toWeighted : '{e, Gen} a ->{e} Weighted a
test.deprecated.internals.v1.foldReport : (Trie Text Status ->{e} r) -> Report ->{e} r
test.deprecated.internals.v1.foldScope : ([Text] ->{g} r) -> Labels ->{g} r
test.deprecated.internals.v1.foldStatus : r
                                          -> (Success ->{g} r)
                                          -> (Success ->{g} r)
                                          -> r
                                          -> Status
                                          ->{g} r
test.deprecated.internals.v1.foldSuccess : (Nat ->{g} r) -> r -> Success ->{g} r
test.deprecated.internals.v1.Scope.cons : Text -> Labels -> Labels
test.deprecated.internals.v1.Status.combine : Status -> Status -> Status
test.deprecated.internals.v1.Status.pending : Status -> Status
test.deprecated.internals.v1.Success.combine : Success -> Success -> Success
test.deprecated.laws.absorption : '{Gen} a
                                  -> (a ->{e} a ->{e} a)
                                  -> (a ->{e} a ->{e} a)
test.deprecated.laws.adjoint : '{Gen} o
                               -> (o ->{g1} o ->{g2} t)
                               -> (o ->{g3} o ->{g4} o)
                               -> (o ->{g5} o ->{g6} o)
test.deprecated.laws.distributive : '{Gen} a
                                    -> (a ->{e} a ->{e} a)
                                    -> (a ->{e} a ->{e} a)
test.deprecated.laws.homomorphism : '{Gen} a
                                    -> (a ->{e} b)
                                    -> (a ->{e} a ->{e} a)
                                    -> (b ->{e} b ->{e} b)
test.deprecated.laws.lattice : '{Gen} a
                               -> (a ->{e} a ->{e} a)
                               -> (a ->{e} a ->{e} a)
test.deprecated.sample : Nat -> '{e, Gen} a ->{e} [a]
test.deprecated.verifyWithSeedAndIgnore : Nat
                                          -> '{g, Exception, Each, Random, Label} a
test.ensure : Boolean ->{Exception} ()
test.ensureEqual : a -> a ->{Exception} ()
test.ensureGreater : x -> x ->{Exception} ()
test.ensureGreaterOrEqual : x -> x ->{Exception} ()
test.ensureLess : x -> x ->{Exception} ()
test.ensureLessOrEqual : x -> x ->{Exception} ()
test.ensureNotEqual : a -> a ->{Exception} ()
test.ensureWith : a -> Boolean ->{Exception} ()
test.ensuring : '{g} Boolean ->{g, Exception} ()
test.ensuringWith : Text -> a -> '{g} Boolean ->{g, Exception} ()
test.laws.abelianGroup : '{Each, Random} t
                         -> (t ->{e} t ->{e1} t)
                         -> t
                         -> (t ->{e2} t)
                         ->{e2, e1, e, Exception, Each, Random, Label} ()
test.laws.absorption : '{Each, Random} a
                       -> (a ->{e} a ->{e} a)
                       -> (a ->{e} a ->{e} a)
                       ->{e, Exception, Each, Random, Label} ()
test.laws.associativity : '{Each, Random} a
                          -> (a ->{e} a ->{e} a)
                          ->{e, Exception, Each, Random, Label} ()
test.laws.commutativity : '{Each, Random} a
                          -> (a ->{e} a ->{e} a)
                          ->{e, Exception, Each, Random, Label} ()
test.laws.distributivity : '{Each, Random} a
                           -> (a ->{e} a ->{e} a)
                           -> (a ->{e} a ->{e} a)
                           ->{e, Exception, Each, Random, Label} ()
test.laws.group : '{Each, Random} t
                  -> (t ->{e} t ->{e1} t)
                  -> t
                  -> (t ->{e2} t)
                  ->{e2, e1, e, Exception, Each, Random, Label} ()
test.laws.homomorphism : '{Each, Random} a
                         -> (a ->{e} a ->{e} a)
                         -> (b ->{e} b ->{e} b)
                         -> (a ->{e} b)
                         ->{e, Exception, Each, Random, Label} ()
test.laws.idempotence : '{Each, Random} a
                        -> (a ->{e} a)
                        ->{e, Exception, Each, Random, Label} ()
test.laws.identity : '{Each, Random} a
                     -> (a ->{e} a ->{e} a)
                     -> a
                     ->{e, Exception, Each, Random, Label} ()
test.laws.involutive : '{Each, Random} a
                       -> (a ->{e} a)
                       ->{e, Exception, Each, Random, Label} ()
test.laws.lattice : '{Each, Random} a
                    -> (a ->{e} a ->{e} a)
                    -> (a ->{e} a ->{e} a)
                    ->{e, Exception, Each, Random, Label} ()
test.laws.monoid : '{Each, Random} t
                   -> (t ->{e} t ->{e1} t)
                   -> t
                   ->{e1, e, Exception, Each, Random, Label} ()
test.laws.reflexivity : '{Each, Random} a
                        -> (a ->{e} a ->{e} Boolean)
                        ->{e, Exception, Each, Random, Label} ()
test.laws.ring : '{Each, Random} t
                 -> (t ->{e4} t ->{e3} t)
                 -> t
                 -> (t ->{e2} t)
                 -> (t ->{e1} t ->{e} t)
                 -> t
                 ->{e4, e3, e2, e1, e, Exception, Each, Random, Label} ()
test.laws.transitivity : '{Each, Random} a
                         -> (a ->{e} a ->{e} Boolean)
                         ->{e, Exception, Each, Random, Label} ()
test.raiseFailure : Text -> a ->{Exception} x
type test.Result
test.Result.Fail : Text -> Result
test.Result.isOk : Result -> Boolean
test.Result.Ok : Text -> Result
test.Result.text : Result -> Text
builtin type Text
Text.!= : Text -> Text -> Boolean
Text.++ : Text -> Text -> Text
Text.< : Text -> Text -> Boolean
Text.<= : Text -> Text -> Boolean
Text.== : Text -> Text -> Boolean
Text.> : Text -> Text -> Boolean
Text.>= : Text -> Text -> Boolean
Text.alignCenterWith : Nat -> Char -> Text -> Text
Text.alignLeftWith : Nat -> Char -> Text -> Text
Text.alignRightWith : Nat -> Char -> Text -> Text
Text.allSplits : Text -> [(Text, Text)]
Text.break : (Char ->{g} Boolean) -> Text ->{g} (Text, Text)
Text.captureAll : Pattern Text -> Text -> [Text]
Text.captureAllWith : ([Text] ->{g} a) -> Pattern Text -> Text ->{g} [a]
Text.capturePatternsWith : [(([Text] ->{g} a), Pattern Text)] -> Text ->{g} [a]
Text.charAt : Nat -> Text -> Optional Char
Text.chunk : Nat -> Text -> [Text]
Text.codePoints : Text -> [Nat]
Text.cons : Char -> Text -> Text
Text.contains : Text -> Text -> Boolean
Text.crlfToLf : Text -> Text
Text.drop : Nat -> Text -> Text
Text.dropRightWhile : (Char ->{g} Boolean) -> Text ->{g} Text
Text.dropUntil : (Char ->{e} Boolean) -> Text ->{e} Text
Text.dropWhile : (Char ->{g} Boolean) -> Text ->{g} Text
Text.empty : Text
Text.endsWith : Text -> Text -> Boolean
Text.eq : Text -> Text -> Boolean
Text.filter : (Char ->{g} Boolean) -> Text ->{g} Text
Text.findFirst : Class -> Text -> (Text, Text)
Text.findFirstIndex : Class -> Text -> Nat
Text.findLast : Class -> Text -> (Text, Text)
Text.findLastIndex : Class -> Text -> Nat
Text.flatMap : (Char ->{g} Text) -> Text ->{g} Text
Text.flatMapRight : (Char ->{g} Text) -> Text ->{g} Text
Text.fromAscii : Bytes -> Text
Text.fromCharList : [Char] -> Text
Text.fromSet : Set Char -> Text
Text.fromUtf8 : Bytes ->{Exception} Text
Text.fromUtf8.impl : Bytes -> Either Failure Text
Text.fromUtf8.partial : Bytes ->{Exception} (Text, Bytes)
Text.fromUtf8.stream : '{g, Stream Bytes} r -> '{g, Exception, Stream Text} r
Text.fromUtf8.stream! : '{g, Stream Bytes} r ->{g, Exception, Stream Text} r
Text.gt : Text -> Text -> Boolean
Text.gteq : Text -> Text -> Boolean
Text.head : Text -> Optional Char
Text.indexOf : Text -> Text -> Optional Nat
Text.isEmpty : Text -> Boolean
Text.join : Text -> [Text] -> Text
Text.leftPad : Nat -> Text -> Text -> Text
Text.lfToCrlf : Text -> Text
Text.lines : Text -> [Text]
Text.lt : Text -> Text -> Boolean
Text.lteq : Text -> Text -> Boolean
Text.map : (Char ->{g} Char) -> Text ->{g} Text
Text.neq : Text -> Text -> Boolean
Text.nonempty : Text -> Optional Text
Text.patterns.anyChar : Pattern Text
Text.patterns.asciiLetter : Pattern Text
Text.patterns.captureWithin : Text -> Text -> Pattern Text
Text.patterns.char : Class -> Pattern Text
Text.patterns.charIn : [Char] -> Pattern Text
Text.patterns.charRange : Char -> Char -> Pattern Text
Text.patterns.chars : Text -> Pattern Text
Text.patterns.charUntil : Class -> Text -> Pattern Text
Text.patterns.digit : Pattern Text
Text.patterns.eof : Pattern Text
Text.patterns.hexDigit : Pattern Text
Text.patterns.isFullMatch : Pattern Text -> Text -> Boolean
Text.patterns.letter : Pattern Text
Text.patterns.literal : Text -> Pattern Text
Text.patterns.notCharIn : [Char] -> Pattern Text
Text.patterns.notCharRange : Char -> Char -> Pattern Text
Text.patterns.notChars : Text -> Pattern Text
Text.patterns.punctuation : Pattern Text
Text.patterns.space : Pattern Text
Text.patterns.wordChar : Pattern Text
Text.repeat : Nat -> Text -> Text
Text.replaceAll : Text -> Text -> Text -> Text
Text.reverse : Text -> Text
Text.segmentBy : (Char ->{e} Boolean) -> Text ->{e} [Text]
Text.size : Text -> Nat
Text.slice : Nat -> Nat -> Text -> Text
Text.snoc : Text -> Char -> Text
Text.split : Char -> Text -> [Text]
Text.split.examples.ex1 : [Text]
Text.split.examples.ex2 : [Text]
Text.splitAt : Nat -> Text -> (Text, Text)
Text.splitOn : Class -> Text -> [Text]
Text.splitOnNewline : Text -> [Text]
Text.splitOnPattern : Pattern Text -> Text -> [Text]
Text.splitOnText : Text -> Text -> [Text]
Text.startsWith : Text -> Text -> Boolean
Text.substitute : ([Text] ->{g} Text) -> Pattern Text -> Text ->{g} Text
Text.substituteAll : Pattern Text -> ([Text] ->{g} Text) -> Text ->{g} Text
Text.substituteMany : ([Text] ->{g} Text) -> Pattern Text -> Text ->{g} Text
Text.take : Nat -> Text -> Text
Text.takeRightWhile : (Char ->{g} Boolean) -> Text ->{g} Text
Text.takeUntil : (Char ->{e} Boolean) -> Text ->{e} Text
Text.takeWhile : (Char ->{e} Boolean) -> Text ->{e} Text
Text.toBag : Text -> Bag Char
Text.toCharList : Text -> [Char]
Text.toInt : Text -> Optional Int
Text.toLowercase : Text -> Text
Text.toNat : Text -> Optional Nat
Text.toSet : Text -> Set Char
Text.toStream : Text ->{Stream Char} ()
Text.toUppercase : Text -> Text
Text.toUtf8 : Text -> Bytes
Text.trim : Text -> Text
Text.uncons : Text -> Optional (Char, Text)
Text.unlines : [Text] -> Text
Text.unsnoc : Text -> Optional (Text, Char)
Text.unwords : [Text] -> Text
Text.words : Text -> [Text]
time.Clock.internals.monotonic.impl : '{IO} Either Failure TimeSpec
time.Clock.internals.processCPUTime.impl : '{IO} Either Failure TimeSpec
time.Clock.internals.realtime.impl : '{IO} Either Failure TimeSpec
time.Clock.internals.systemTimeZone.impl : Int ->{IO} (Int, Nat, Text)
time.Clock.internals.threadCPUTime.impl : '{IO} Either Failure TimeSpec
builtin type time.Clock.internals.TimeSpec
time.Clock.internals.TimeSpec.nsec : TimeSpec -> Nat
time.Clock.internals.TimeSpec.sec : TimeSpec -> Int
time.Clock.monotonic : '{IO, Exception} Duration
time.Clock.processCPUTime : '{IO, Exception} Duration
time.Clock.realtime : '{IO, Exception} Instant
time.Clock.threadCPUTime : '{IO, Exception} Duration
time.Clock.timeSinceEpoch : '{IO, Exception} Duration
type time.DayOfWeek
time.DayOfWeek.daysSinceSat : DayOfWeek -> Nat
time.DayOfWeek.Fri : DayOfWeek
time.DayOfWeek.fromName : Text -> Optional DayOfWeek
time.DayOfWeek.isWeekday : DayOfWeek -> Boolean
time.DayOfWeek.isWeekend : DayOfWeek -> Boolean
time.DayOfWeek.Mon : DayOfWeek
time.DayOfWeek.name : DayOfWeek -> Text
time.DayOfWeek.next : DayOfWeek -> DayOfWeek
time.DayOfWeek.number : Nat -> DayOfWeek
time.DayOfWeek.previous : DayOfWeek -> DayOfWeek
time.DayOfWeek.Sat : DayOfWeek
time.DayOfWeek.shortName : DayOfWeek -> Text
time.DayOfWeek.Sun : DayOfWeek
time.DayOfWeek.Thu : DayOfWeek
time.DayOfWeek.Tue : DayOfWeek
time.DayOfWeek.Wed : DayOfWeek
type time.Duration
time.Duration.!= : Duration -> Duration -> Boolean
time.Duration.* : Int -> Duration -> Duration
time.Duration.+ : Duration -> Duration -> Duration
time.Duration.- : Duration -> Duration -> Duration
time.Duration./ : Duration -> Int -> Duration
time.Duration.< : Duration -> Duration -> Boolean
time.Duration.<= : Duration -> Duration -> Boolean
time.Duration.== : Duration -> Duration -> Boolean
time.Duration.> : Duration -> Duration -> Boolean
time.Duration.>= : Duration -> Duration -> Boolean
time.Duration.abs : Duration -> Duration
time.Duration.asDays : Duration -> Float
time.Duration.asHours : Duration -> Float
time.Duration.asJulianYears : Duration -> Float
time.Duration.asMicroseconds : Duration -> Float
time.Duration.asMilliseconds : Duration -> Float
time.Duration.asMinutes : Duration -> Float
time.Duration.asNanoseconds : Duration -> Int
time.Duration.asSeconds : Duration -> Float
time.Duration.asWeeks : Duration -> Float
time.Duration.asYears : Duration -> Float
time.Duration.averageMonth : Duration
time.Duration.averageMonths : Int -> Duration
time.Duration.averageYear : Duration
time.Duration.averageYears : Int -> Duration
time.Duration.between : Instant -> Instant -> Duration
time.Duration.compare : Duration -> Duration -> Ordering
time.Duration.countDays : Duration -> Int
time.Duration.countHours : Duration -> Int
time.Duration.countJulianYears : Duration -> Int
time.Duration.countMicroseconds : Duration -> Int
time.Duration.countMilliseconds : Duration -> Int
time.Duration.countMinutes : Duration -> Int
time.Duration.countSeconds : Duration -> Int
time.Duration.countWeeks : Duration -> Int
time.Duration.countYears : Duration -> Int
time.Duration.day : Duration
time.Duration.days : Int -> Duration
time.Duration.div : Duration -> Int -> Duration
time.Duration.fortnight : Duration
time.Duration.hour : Duration
time.Duration.hours : Int -> Duration
time.Duration.internal.Duration : Int -> Nat -> Duration
time.Duration.isNegative : Duration -> Boolean
time.Duration.isZero : Duration -> Boolean
time.Duration.julianYear : Duration
time.Duration.julianYears : Int -> Duration
time.Duration.maxDuration : Duration
time.Duration.microsecond : Duration
time.Duration.microseconds : Int -> Duration
time.Duration.millisecond : Duration
time.Duration.milliseconds : Int -> Duration
time.Duration.minDuration : Duration
time.Duration.minute : Duration
time.Duration.minutes : Int -> Duration
time.Duration.mod : Duration -> Duration ->{Exception} Duration
time.Duration.nanosComponent : Duration -> Nat
time.Duration.nanosecond : Duration
time.Duration.nanoseconds : Int -> Duration
time.Duration.negate : Duration -> Duration
time.Duration.second : Duration
time.Duration.seconds : Int -> Duration
time.Duration.setNanos : Duration -> Nat -> Duration
time.Duration.setSeconds : Duration -> Int -> Duration
time.Duration.toText : Duration -> Text
time.Duration.week : Duration
time.Duration.weeks : Int -> Duration
time.Duration.zero : Duration
type time.Instant
time.Instant.!= : Instant -> Instant -> Boolean
time.Instant.+ : Instant -> Duration -> Instant
time.Instant.- : Instant -> Duration -> Instant
time.Instant.< : Instant -> Instant -> Boolean
time.Instant.<= : Instant -> Instant -> Boolean
time.Instant.== : Instant -> Instant -> Boolean
time.Instant.> : Instant -> Instant -> Boolean
time.Instant.>= : Instant -> Instant -> Boolean
time.Instant.addDuration : Instant -> Duration -> Instant
time.Instant.addYears : Instant -> Int -> Instant
time.Instant.atOffset : Instant -> UTCOffset -> OffsetDateTime
time.Instant.atUTC : Instant -> OffsetDateTime
time.Instant.ceiling : Duration -> Instant ->{Exception} Instant
time.Instant.compare : Instant -> Instant -> Ordering
time.Instant.epoch : Instant
time.Instant.floor : Duration -> Instant ->{Exception} Instant
time.Instant.fromEpochMicroseconds : Int -> Instant
time.Instant.fromEpochMilliseconds : Int -> Instant
time.Instant.fromEpochNanoseconds : Int -> Instant
time.Instant.fromEpochSeconds : Int -> Instant
time.Instant.fromIso8601 : Text -> Optional Instant
time.Instant.internal.dayOfYearToMonthAndDay : Boolean -> Nat -> (Nat, Nat)
time.Instant.internal.Instant : Int -> Nat -> Instant
time.Instant.microsecondsSinceEpoch : Instant -> Int
time.Instant.millisecondsSinceEpoch : Instant -> Int
time.Instant.nanosecondOfSecond : Instant -> Nat
time.Instant.nanosecondOfSecond.modify : (Nat ->{g} Nat) -> Instant ->{g} Instant
time.Instant.nanosecondOfSecond.set : Nat -> Instant -> Instant
time.Instant.nanosecondsSinceEpoch : Instant -> Int
time.Instant.now : '{IO, Exception} Instant
time.Instant.round : Duration -> Instant ->{Exception} Instant
time.Instant.secondsSinceEpoch : Instant -> Int
time.Instant.secondsSinceEpoch.modify : (Int ->{g} Int) -> Instant ->{g} Instant
time.Instant.secondsSinceEpoch.set : Int -> Instant -> Instant
time.Instant.subtractDuration : Instant -> Duration -> Instant
time.Instant.timeSinceEpoch : Instant -> Duration
time.Instant.toBasicISO8601 : Instant -> Text
time.Instant.toRFC1123 : Instant -> Text
time.Instant.toRFC2822 : Instant -> Text
time.Instant.toText : Instant -> Text
time.Instant.truncateToCenturies : Instant -> Instant
time.Instant.truncateToDays : Instant -> Instant
time.Instant.truncateToHours : Instant -> Instant
time.Instant.truncateToMicroseconds : Instant -> Instant
time.Instant.truncateToMillennia : Instant -> Instant
time.Instant.truncateToMilliseconds : Instant -> Instant
time.Instant.truncateToMinutes : Instant -> Instant
time.Instant.truncateToSeconds : Instant -> Instant
time.Instant.truncateToYears : Instant -> Instant
time.isLeapYear : Int -> Boolean
type time.LocalDate
time.LocalDate.!= : LocalDate -> LocalDate -> Boolean
time.LocalDate.+ : LocalDate -> Int -> LocalDate
time.LocalDate.- : LocalDate -> LocalDate -> Int
time.LocalDate.< : LocalDate -> LocalDate -> Boolean
time.LocalDate.<= : LocalDate -> LocalDate -> Boolean
time.LocalDate.== : LocalDate -> LocalDate -> Boolean
time.LocalDate.> : LocalDate -> LocalDate -> Boolean
time.LocalDate.>= : LocalDate -> LocalDate -> Boolean
time.LocalDate.addDuration : LocalDate -> Duration -> LocalDate
time.LocalDate.atTime : LocalDate -> LocalTime -> LocalDateTime
time.LocalDate.current : '{IO, Exception} LocalDate
time.LocalDate.day : LocalDate -> Nat
time.LocalDate.day.modify : (Nat ->{g} Nat) -> LocalDate ->{g} LocalDate
time.LocalDate.day.set : Nat -> LocalDate -> LocalDate
time.LocalDate.dayOfWeek : LocalDate -> DayOfWeek
time.LocalDate.duration : LocalDate -> LocalDate -> Duration
time.LocalDate.format : Text -> LocalDate -> Text
time.LocalDate.fromBasicISO8601 : Text -> Optional LocalDate
time.LocalDate.fromIso8601 : Text -> Optional LocalDate
time.LocalDate.fromRFC7231 : Text ->{Exception} LocalDate
time.LocalDate.LocalDate : Int -> Nat -> Nat -> LocalDate
time.LocalDate.month : LocalDate -> Nat
time.LocalDate.month.modify : (Nat ->{g} Nat) -> LocalDate ->{g} LocalDate
time.LocalDate.month.set : Nat -> LocalDate -> LocalDate
time.LocalDate.range : LocalDate -> LocalDate -> [LocalDate]
time.LocalDate.toBasicISO8601 : LocalDate -> Text
time.LocalDate.toText : LocalDate -> Text
time.LocalDate.year : LocalDate -> Int
time.LocalDate.year.modify : (Int ->{g} Int) -> LocalDate ->{g} LocalDate
time.LocalDate.year.set : Int -> LocalDate -> LocalDate
type time.LocalDateTime
time.LocalDateTime.!= : LocalDateTime -> LocalDateTime -> Boolean
time.LocalDateTime.< : LocalDateTime -> LocalDateTime -> Boolean
time.LocalDateTime.<= : LocalDateTime -> LocalDateTime -> Boolean
time.LocalDateTime.== : LocalDateTime -> LocalDateTime -> Boolean
time.LocalDateTime.> : LocalDateTime -> LocalDateTime -> Boolean
time.LocalDateTime.>= : LocalDateTime -> LocalDateTime -> Boolean
time.LocalDateTime.addDuration : LocalDateTime -> Duration -> LocalDateTime
time.LocalDateTime.atOffset : LocalDateTime -> UTCOffset -> OffsetDateTime
time.LocalDateTime.atStartOfDay : LocalDate -> LocalDateTime
time.LocalDateTime.current : '{IO, Exception} LocalDateTime
time.LocalDateTime.date : LocalDateTime -> LocalDate
time.LocalDateTime.date.modify : (LocalDate ->{g} LocalDate)
                                 -> LocalDateTime
                                 ->{g} LocalDateTime
time.LocalDateTime.date.set : LocalDate -> LocalDateTime -> LocalDateTime
time.LocalDateTime.dayOfWeek : LocalDateTime -> DayOfWeek
time.LocalDateTime.format : Text -> LocalDateTime -> Text
time.LocalDateTime.fromIso8601 : Text -> Optional LocalDateTime
time.LocalDateTime.fromRFC7231 : Text ->{Exception} LocalDateTime
time.LocalDateTime.LocalDateTime : LocalDate -> LocalTime -> LocalDateTime
time.LocalDateTime.time : LocalDateTime -> LocalTime
time.LocalDateTime.time.modify : (LocalTime ->{g} LocalTime)
                                 -> LocalDateTime
                                 ->{g} LocalDateTime
time.LocalDateTime.time.set : LocalTime -> LocalDateTime -> LocalDateTime
time.LocalDateTime.toBasicISO8601 : LocalDateTime -> Text
time.LocalDateTime.toRFC1123AtGMT : LocalDateTime -> Text
time.LocalDateTime.toText : LocalDateTime -> Text
type time.LocalTime
time.LocalTime.!= : LocalTime -> LocalTime -> Boolean
time.LocalTime.< : LocalTime -> LocalTime -> Boolean
time.LocalTime.<= : LocalTime -> LocalTime -> Boolean
time.LocalTime.== : LocalTime -> LocalTime -> Boolean
time.LocalTime.> : LocalTime -> LocalTime -> Boolean
time.LocalTime.>= : LocalTime -> LocalTime -> Boolean
time.LocalTime.addDuration : LocalTime -> Duration -> LocalTime
time.LocalTime.atOffset : LocalTime -> UTCOffset -> OffsetTime
time.LocalTime.current : '{IO, Exception} LocalTime
time.LocalTime.fromBasicISO8601 : Text -> Optional LocalTime
time.LocalTime.fromIso8601 : Text -> Optional LocalTime
time.LocalTime.fromRFC7231 : Text ->{Exception} LocalTime
time.LocalTime.hour : LocalTime -> Nat
time.LocalTime.hour.modify : (Nat ->{g} Nat) -> LocalTime ->{g} LocalTime
time.LocalTime.hour.set : Nat -> LocalTime -> LocalTime
time.LocalTime.LocalTime : Nat -> Nat -> Nat -> Nat -> LocalTime
time.LocalTime.minute : LocalTime -> Nat
time.LocalTime.minute.modify : (Nat ->{g} Nat) -> LocalTime ->{g} LocalTime
time.LocalTime.minute.set : Nat -> LocalTime -> LocalTime
time.LocalTime.nanosecond : LocalTime -> Nat
time.LocalTime.nanosecond.modify : (Nat ->{g} Nat) -> LocalTime ->{g} LocalTime
time.LocalTime.nanosecond.set : Nat -> LocalTime -> LocalTime
time.LocalTime.second : LocalTime -> Nat
time.LocalTime.second.modify : (Nat ->{g} Nat) -> LocalTime ->{g} LocalTime
time.LocalTime.second.set : Nat -> LocalTime -> LocalTime
time.LocalTime.toBasicISO8601 : LocalTime -> Text
time.LocalTime.toText : LocalTime -> Text
time.monthLengths : Boolean -> List.Nonempty Nat
time.monthNames : [Text]
time.monthNamesShort : [Text]
structural type time.OffsetDateTime
time.OffsetDateTime.!= : OffsetDateTime -> OffsetDateTime -> Boolean
time.OffsetDateTime.< : OffsetDateTime -> OffsetDateTime -> Boolean
time.OffsetDateTime.<= : OffsetDateTime -> OffsetDateTime -> Boolean
time.OffsetDateTime.== : OffsetDateTime -> OffsetDateTime -> Boolean
time.OffsetDateTime.> : OffsetDateTime -> OffsetDateTime -> Boolean
time.OffsetDateTime.>= : OffsetDateTime -> OffsetDateTime -> Boolean
time.OffsetDateTime.addDuration : OffsetDateTime -> Duration -> OffsetDateTime
time.OffsetDateTime.convertOffset : UTCOffset -> OffsetDateTime -> OffsetDateTime
time.OffsetDateTime.current : '{IO, Exception} OffsetDateTime
time.OffsetDateTime.date : OffsetDateTime -> LocalDate
time.OffsetDateTime.dayOfWeek : OffsetDateTime -> DayOfWeek
time.OffsetDateTime.fromInstant : Instant -> UTCOffset -> OffsetDateTime
time.OffsetDateTime.fromIso8601 : Text -> Optional OffsetDateTime
time.OffsetDateTime.fromRFC1123 : Text ->{Exception} OffsetDateTime
time.OffsetDateTime.fromRFC2822 : Text ->{Exception} OffsetDateTime
time.OffsetDateTime.fromTimeAndDate : OffsetTime -> LocalDate -> OffsetDateTime
time.OffsetDateTime.localDateTime : OffsetDateTime -> LocalDateTime
time.OffsetDateTime.offset : OffsetDateTime -> UTCOffset
time.OffsetDateTime.OffsetDateTime : UTCOffset -> LocalDateTime -> OffsetDateTime
time.OffsetDateTime.timeOfDay : OffsetDateTime -> OffsetTime
time.OffsetDateTime.toBasicISO8601 : OffsetDateTime -> Text
time.OffsetDateTime.toInstant : OffsetDateTime -> Instant
time.OffsetDateTime.toRFC1123 : OffsetDateTime -> Text
time.OffsetDateTime.toRFC2822 : OffsetDateTime -> Text
time.OffsetDateTime.toText : OffsetDateTime -> Text
time.OffsetDateTime.toUTC : OffsetDateTime -> OffsetDateTime
time.OffsetDateTime.toUTCLocal : OffsetDateTime -> LocalDateTime
type time.OffsetTime
time.OffsetTime.!= : OffsetTime -> OffsetTime -> Boolean
time.OffsetTime.< : OffsetTime -> OffsetTime -> Boolean
time.OffsetTime.<= : OffsetTime -> OffsetTime -> Boolean
time.OffsetTime.== : OffsetTime -> OffsetTime -> Boolean
time.OffsetTime.> : OffsetTime -> OffsetTime -> Boolean
time.OffsetTime.>= : OffsetTime -> OffsetTime -> Boolean
time.OffsetTime.addDuration : OffsetTime -> Duration -> OffsetTime
time.OffsetTime.fromIso8601 : Text -> Optional OffsetTime
time.OffsetTime.offset : OffsetTime -> UTCOffset
time.OffsetTime.offset.modify : (UTCOffset ->{g} UTCOffset) -> OffsetTime ->{g} OffsetTime
time.OffsetTime.offset.set : UTCOffset -> OffsetTime -> OffsetTime
time.OffsetTime.OffsetTime : UTCOffset -> LocalTime -> OffsetTime
time.OffsetTime.time : OffsetTime -> LocalTime
time.OffsetTime.time.modify : (LocalTime ->{g} LocalTime) -> OffsetTime ->{g} OffsetTime
time.OffsetTime.time.set : LocalTime -> OffsetTime -> OffsetTime
time.OffsetTime.toText : OffsetTime -> Text
time.patterns.asctimeFormat : Pattern Text
time.patterns.iso8601Date : Pattern Text
time.patterns.iso8601DateTime : Pattern Text
time.patterns.iso8601LocalDateTime : Pattern Text
time.patterns.iso8601LocalTime : Pattern Text
time.patterns.iso8601Time : Pattern Text
time.patterns.iso8601Timezone : Pattern Text
time.patterns.militaryTimeZones : [Text]
time.patterns.rfc2822DateTime : Pattern Text
time.patterns.rfc2822Offset : Pattern Text
time.patterns.rfc2822Time : Pattern Text
time.patterns.rfc7231Date : Pattern Text
time.patterns.rfc7231DateTime : Pattern Text
time.patterns.rfc7231Time : Pattern Text
type time.TimeZone
time.TimeZone.currentTimeZone : '{IO, Exception} TimeZone
time.TimeZone.getTimeZone : Instant ->{IO} TimeZone
time.TimeZone.name : TimeZone -> Text
time.TimeZone.name.modify : (Text ->{g} Text) -> TimeZone ->{g} TimeZone
time.TimeZone.name.set : Text -> TimeZone -> TimeZone
time.TimeZone.offset : TimeZone -> UTCOffset
time.TimeZone.offset.modify : (UTCOffset ->{g} UTCOffset) -> TimeZone ->{g} TimeZone
time.TimeZone.offset.set : UTCOffset -> TimeZone -> TimeZone
time.TimeZone.summerOnly : TimeZone -> Boolean
time.TimeZone.summerOnly.modify : (Boolean ->{g} Boolean) -> TimeZone ->{g} TimeZone
time.TimeZone.summerOnly.set : Boolean -> TimeZone -> TimeZone
time.TimeZone.TimeZone : UTCOffset -> Boolean -> Text -> TimeZone
time.TimeZone.toDuration : TimeZone -> Duration
type time.UTCOffset
time.UTCOffset.addHours : Nat -> UTCOffset -> UTCOffset
time.UTCOffset.addHours.flipped : UTCOffset -> Nat -> UTCOffset
time.UTCOffset.addMinutes : Nat -> UTCOffset -> UTCOffset
time.UTCOffset.currentOffset : '{IO, Exception} UTCOffset
time.UTCOffset.fromBasicISO8601 : Text -> Optional UTCOffset
time.UTCOffset.fromHours : Int -> UTCOffset
time.UTCOffset.fromIso8601 : Text -> Optional UTCOffset
time.UTCOffset.fromMinutes : Int -> UTCOffset
time.UTCOffset.fromRFC2822 : Text ->{Exception} UTCOffset
time.UTCOffset.getOffset : Instant ->{IO} UTCOffset
time.UTCOffset.isNegative : UTCOffset -> Boolean
time.UTCOffset.offsetHours : UTCOffset -> Int
time.UTCOffset.offsetMinutes : UTCOffset -> Nat
time.UTCOffset.offsetNamed.ADT : UTCOffset
time.UTCOffset.offsetNamed.AKDT : UTCOffset
time.UTCOffset.offsetNamed.AKST : UTCOffset
time.UTCOffset.offsetNamed.ART : UTCOffset
time.UTCOffset.offsetNamed.AST : UTCOffset
time.UTCOffset.offsetNamed.BRST : UTCOffset
time.UTCOffset.offsetNamed.BRT : UTCOffset
time.UTCOffset.offsetNamed.BST : UTCOffset
time.UTCOffset.offsetNamed.CDT : UTCOffset
time.UTCOffset.offsetNamed.CEST : UTCOffset
time.UTCOffset.offsetNamed.CET : UTCOffset
time.UTCOffset.offsetNamed.CST : UTCOffset
time.UTCOffset.offsetNamed.DST : UTCOffset
time.UTCOffset.offsetNamed.EAT : UTCOffset
time.UTCOffset.offsetNamed.EDT : UTCOffset
time.UTCOffset.offsetNamed.EEST : UTCOffset
time.UTCOffset.offsetNamed.EET : UTCOffset
time.UTCOffset.offsetNamed.EST : UTCOffset
time.UTCOffset.offsetNamed.GMT : UTCOffset
time.UTCOffset.offsetNamed.GST : UTCOffset
time.UTCOffset.offsetNamed.HST : UTCOffset
time.UTCOffset.offsetNamed.IDLE : UTCOffset
time.UTCOffset.offsetNamed.IDLW : UTCOffset
time.UTCOffset.offsetNamed.IST : UTCOffset
time.UTCOffset.offsetNamed.JST : UTCOffset
time.UTCOffset.offsetNamed.MDT : UTCOffset
time.UTCOffset.offsetNamed.MSD : UTCOffset
time.UTCOffset.offsetNamed.MSK : UTCOffset
time.UTCOffset.offsetNamed.MST : UTCOffset
time.UTCOffset.offsetNamed.NDT : UTCOffset
time.UTCOffset.offsetNamed.NZDT : UTCOffset
time.UTCOffset.offsetNamed.NZST : UTCOffset
time.UTCOffset.offsetNamed.NZT : UTCOffset
time.UTCOffset.offsetNamed.PDT : UTCOffset
time.UTCOffset.offsetNamed.PKT : UTCOffset
time.UTCOffset.offsetNamed.PST : UTCOffset
time.UTCOffset.offsetNamed.SLT : UTCOffset
time.UTCOffset.offsetNamed.SST : UTCOffset
time.UTCOffset.offsetNamed.UTC : UTCOffset
time.UTCOffset.offsetNamed.WAT : UTCOffset
time.UTCOffset.offsetNamed.WET : UTCOffset
time.UTCOffset.offsetNamed.WIB : UTCOffset
time.UTCOffset.offsetNamed.WST : UTCOffset
time.UTCOffset.subtractHours : Nat -> UTCOffset -> UTCOffset
time.UTCOffset.subtractHours.flipped : UTCOffset -> Nat -> UTCOffset
time.UTCOffset.subtractMinutes : Nat -> UTCOffset -> UTCOffset
time.UTCOffset.toBasicISO8601 : UTCOffset -> Text
time.UTCOffset.toDuration : UTCOffset -> Duration
time.UTCOffset.toHours : UTCOffset -> Float
time.UTCOffset.toMinutes : UTCOffset -> Int
time.UTCOffset.toMinutes.modify : (Int ->{g} Int) -> UTCOffset ->{g} UTCOffset
time.UTCOffset.toMinutes.set : Int -> UTCOffset -> UTCOffset
time.UTCOffset.toRFC2822 : UTCOffset -> Text
time.UTCOffset.toText : UTCOffset -> Text
time.UTCOffset.utc : Int -> UTCOffset
time.UTCOffset.UTCOffset : Int -> UTCOffset
todo : a -> b
structural type Unit
Unit.Unit : ()
Universal.!== : a -> a -> Boolean
Universal.=== : a -> a -> Boolean
Universal.compare : a -> a -> Int
Universal.compareOn : (a ->{e} x) -> a -> a ->{e} Ordering
Universal.eq : a -> a -> Boolean
Universal.gt : a -> a -> Boolean
Universal.gteq : a -> a -> Boolean
Universal.lt : a -> a -> Boolean
Universal.lteq : a -> a -> Boolean
Universal.max : a -> a -> a
Universal.min : a -> a -> a
Universal.murmurHash : a -> Nat
Universal.neq : a -> a -> Boolean
Universal.ordering : a -> a -> Ordering
Universal.ordering.rewriteToCompare : x
                                      -> x
                                      -> lt
                                      -> lt
                                      -> lt
                                      -> Rewrites
                                        ( RewriteTerm lt lt,
                                          RewriteTerm lt lt,
                                          RewriteTerm lt lt,
                                          RewriteTerm lt lt)
unsafe.coerceAbilities : (a ->{e1} b) -> a -> b
structural type Void
Void.absurd : Void -> a
Void.absurdly : '{e} Void ->{e} a
```
