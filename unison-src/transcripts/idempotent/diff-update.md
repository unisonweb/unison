# diff.update

The `diff.update` command shows a preview of what changes would be made if `update` were run,
displaying an inline diff of modified definitions.

## Setup

```ucm
scratch/main> builtins.merge
```

First, let's add a term to the codebase:

```unison
foo : Nat
foo =
  use Nat +
  x = 1
  y = 2
  x + y
```

```ucm
scratch/main> add
```

## Test diff.update with a modified term

Now let's modify the term in the scratch file:

```unison
foo : Nat
foo =
  use Nat +
  x = 1
  y = 3
  x + y
```

Running `diff.update` should show an inline diff with the changed line:

```ucm
scratch/main> diff.update
```

## Test diff.update with a new term

Let's also add a completely new term:

```unison
foo : Nat
foo =
  use Nat +
  x = 1
  y = 3
  x + y

bar : Nat
bar = 42
```

Running `diff.update` should show both the modified term and the new term:

```ucm
scratch/main> diff.update
```

