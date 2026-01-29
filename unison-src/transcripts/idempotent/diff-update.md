# diff.update

The `diff.update` command shows a preview of what changes would be made if `update` were run,
displaying an inline diff of modified definitions.

## Setup

``` ucm
scratch/main> builtins.merge

  Done.
```

First, let's add a term to the codebase:

``` unison
foo : Nat
foo =
  use Nat +
  x = 1
  y = 2
  x + y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Test diff.update with a modified term

Now let's modify the term in the scratch file:

``` unison
foo : Nat
foo =
  use Nat +
  x = 1
  y = 3
  x + y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

Running `diff.update` should show an inline diff with the changed line:

``` ucm
scratch/main> diff.update

  Preview of changes that would be made by `update`:

  Updated definitions:
      foo : Nat
      foo =
        use Nat +
        x = 1
    -   y = 2
    +   y = 3
        x + y

  + (added), - (deleted)

  Run `update` to apply these changes.
```

## Test diff.update with a new term

Let's also add a completely new term that depends on `foo`:

``` unison
foo : Nat
foo =
  use Nat +
  x = 1
  y = 3
  x + y

bar : Nat
bar =
  use Nat +
  foo + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  ~ foo : Nat

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

Running `diff.update` should show both the modified term and the new term:

``` ucm
scratch/main> diff.update

  Preview of changes that would be made by `update`:

  New definitions:
    + bar : Nat
    + bar =
    +   use Nat +
    +   foo + 1

  Updated definitions:
      foo : Nat
      foo =
        use Nat +
        x = 1
    -   y = 2
    +   y = 3
        x + y

  + (added), - (deleted)

  Run `update` to apply these changes.
```

Let's apply the update so we can test more scenarios:

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Test diff.update with a new type

Let's test adding a brand new type (not modifying an existing one) to see if diff.update
shows the full type structure or just the type name:

``` unison
structural type Person = { name : Text, age : Nat }

unique ability Counter where
  increment : Nat
  getCount : Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ability Counter
  + structural type Person

  + Person.age         : Person -> Nat
  + Person.age.modify  : (Nat ->{g} Nat) -> Person ->{g} Person
  + Person.age.set     : Nat -> Person -> Person
  + Person.name        : Person -> Text
  + Person.name.modify : (Text ->{g} Text)
                         -> Person
                         ->{g} Person
  + Person.name.set    : Text -> Person -> Person

  Run `update` to apply these changes to your codebase.
```

Running `diff.update` should show the new type with its full structure:

``` ucm
scratch/main> diff.update

  Preview of changes that would be made by `update`:

  New definitions:
    + ability Counter where
    +   increment : {Counter} Nat
    +   getCount : {Counter} Nat
    + structural type Person = { name : Text, age : Nat }
    + Person.age : Person -> Nat
    + Person.age = cases Person _ age -> age
    + Person.age.modify : (Nat ->{g} Nat) -> Person ->{g} Person
    + Person.age.modify f = cases Person name age -> Person name (f age)
    + Person.age.set : Nat -> Person -> Person
    + Person.age.set age1 = cases Person name _ -> Person name age1
    + Person.name : Person -> Text
    + Person.name = cases Person name _ -> name
    + Person.name.modify : (Text ->{g} Text) -> Person ->{g} Person
    + Person.name.modify f = cases Person name age -> Person (f name) age
    + Person.name.set : Text -> Person -> Person
    + Person.name.set name1 = cases Person _ age -> Person name1 age

  Run `update` to apply these changes.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Test diff.update with a modified type

Let's add a type to the codebase:

``` unison
structural type Color = Red | Green | Blue
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Color

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now modify the type by adding a constructor:

``` unison
structural type Color = Red | Green | Blue | Yellow
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ structural type Color

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

Running `diff.update` should show the type change:

``` ucm
scratch/main> diff.update

  Preview of changes that would be made by `update`:

  Updated definitions:
    - structural type Color = Red | Green | Blue
    + structural type Color = Red | Green | Blue | Yellow

  + (added), - (deleted)

  Run `update` to apply these changes.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## Test diff.update with a modified ability

Let's add an ability to the codebase:

``` unison
structural ability Log where
  log : Text -> ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural ability Log

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now modify the ability by adding a new operation:

``` unison
structural ability Log where
  log : Text -> ()
  logLevel : Nat -> Text -> ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ structural ability Log

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

Running `diff.update` should show the ability change:

``` ucm
scratch/main> diff.update

  Preview of changes that would be made by `update`:

  Updated definitions:
      structural ability Log where
        log : Text ->{Log} ()
    +   logLevel : Nat -> Text ->{Log} ()

  + (added), - (deleted)

  Run `update` to apply these changes.
```
