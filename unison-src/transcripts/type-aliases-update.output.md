# Updating a type alias propagates to dependents

When a `type alias` body changes, every term that mentions the alias is
detected as a dependent and re-typechecked against the new alias — same
flow as a decl update.

``` ucm :hide
> builtins.mergeio
```

## Compatible body change: dependents re-hashed cleanly

Define an alias and a term that uses it.

``` unison
type alias Boxed a = Optional a

f : Boxed Nat -> Nat
f b = match b with
  Some n -> n
  None -> 0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Boxed a = Optional a

  + f : Boxed Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now change `Boxed`'s body to a different but compatible alias. `f`
typechecks against the new alias, gets a fresh hash, and its stored type
references the new `Boxed`.

``` unison
type alias Boxed a = Optional a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Boxed a = Optional a

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view f

  f : Boxed Nat -> Nat
  f = cases
    Some n -> n
    None   -> 0
```

## Incompatible body change: update is rejected

If the new alias body breaks dependents, the standard UCM update flow
catches it and creates a fix-up branch — exactly as it would for a decl.

``` unison
type alias Endo a = a -> a

g : Endo Nat
g n = n + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a

  + g : Endo Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Redefining `Endo` to take a second argument means `g`'s body no longer
fits:

``` unison
type alias Endo a = a -> a -> a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a -> a

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  I couldn't complete the update, because some existing
  definitions would no longer typecheck.

  I've created a temporary branch and added the affected
  definitions to scratch.u, where you can fix them up or remove
  any that are obsolete.

  Once you're happy with the results, use `update` to merge them
  back into main, or `cancel` if you change your mind.
```

``` unison :added-by-ucm scratch.u
type alias Endo a = a -> a -> a

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

g : Endo Nat
g n =
  use Nat +
  n + 1

```
