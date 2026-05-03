# `mark.given`, `unmark.given`, and `givens`

These commands toggle and inspect the @\#\#Builtin.Given@ sentinel
stored in namespace metadata (ADR-013). The term hash of the marked
definition is unchanged (ADR-014); only the namespace hash changes.

``` ucm :hide
> builtins.mergeio lib.builtins
```

## Happy path: mark, list, unmark, list

``` unison
foo : Nat
foo = 42

bar : Nat
bar = 99
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  + foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Initially, no givens.

``` ucm
> givens

  No definitions in the current namespace are marked as givens.
```

Mark `foo`.

``` ucm
> mark.given foo

  Marked foo. It will now participate in implicit resolution.
```

Now `givens` lists `foo`.

``` ucm
> givens

  Definitions marked as givens in the current namespace:

    foo
```

The `find :given` filter returns the same set.

``` ucm
> find :given

  1. foo : Nat
```

Unmark `foo`.

``` ucm
> unmark.given foo

  Unmarked foo. It will no longer participate in implicit
  resolution.
```

The list is empty again.

``` ucm
> givens

  No definitions in the current namespace are marked as givens.
```

## Idempotence

Marking an already-marked definition is a no-op with a clear message.

``` ucm
> mark.given foo

  Marked foo. It will now participate in implicit resolution.

> mark.given foo

  foo is already marked as a given. No changes were made.
```

Unmarking returns to the empty state.

``` ucm
> unmark.given foo

  Unmarked foo. It will no longer participate in implicit
  resolution.
```

Unmarking an unmarked definition is a no-op with a clear message.

``` ucm
> unmark.given foo

  foo is not marked as a given. No changes were made.
```

## `view` surfaces a given marker

When a definition is tagged as a given, `view` prepends a marker so
the parser-side @given@ keyword (chunk A2) can round-trip with the
namespace tag. Until A2 lands, the marker is rendered as a leading
@-- given@ comment.

``` ucm
> mark.given foo

  Marked foo. It will now participate in implicit resolution.

> view foo

  -- given
  foo : Nat
  foo = 42
```

``` ucm
> unmark.given foo

  Unmarked foo. It will no longer participate in implicit
  resolution.

> view foo

  foo : Nat
  foo = 42
```

## Error: marking a missing name

``` ucm :error
> mark.given doesNotExist

  ⚠️

  I don't know about that term.
```

## `mark.given` / `unmark.given` from a sub-namespace

`mark.given` and `unmark.given` resolve their argument relative to
the *current* namespace, but the metadata write must land at the
correct absolute path within the project root. Regression test for
the bug where these commands read the parent branch via the
already-deep current branch and so could not see (or could not
toggle off) a marking made from a sub-namespace.

``` unison
myNs.alpha : Nat
myNs.alpha = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + myNs.alpha : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` ucm
> deprecated.cd myNs

> mark.given alpha

  Marked alpha. It will now participate in implicit resolution.
```

A second `mark.given` from the same sub-namespace must report
`AlreadyMarkedGiven`, which only works if the read-side parent
branch lookup uses the project root.

``` ucm
> mark.given alpha

  alpha is already marked as a given. No changes were made.
```

`unmark.given` from the same sub-namespace toggles the marking
off, again exercising the read path.

``` ucm
> unmark.given alpha

  Unmarked alpha. It will no longer participate in implicit
  resolution.

> unmark.given alpha

  alpha is not marked as a given. No changes were made.
```

``` ucm :hide
> deprecated.cd ..
```

## `find :given` finds deeply-nested givens

`find :given` filters by checking the given-sentinel metadata at
each containing namespace, not just at the top level. Regression
test for the bug where the filter consulted only the top-level
'Star2' and so silently dropped givens nested under sub-namespaces.

``` unison
deep.nested.beta : Nat
deep.nested.beta = 7
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + deep.nested.beta : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> mark.given deep.nested.beta

  Marked deep.nested.beta. It will now participate in implicit
  resolution.
```

`find :given` from the parent namespace must surface the
deeply-nested marking.

``` ucm
> find :given

  1. deep.nested.beta : Nat
```

`givens` likewise lists the deeply-nested marking.

``` ucm
> givens

  Definitions marked as givens in the current namespace:

    deep.nested.beta
```
