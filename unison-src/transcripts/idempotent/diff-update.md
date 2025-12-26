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
