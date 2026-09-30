# Updating aliases checks transitive dependents

``` ucm :hide
> builtins.mergeio
```

A compatible update must move both the intermediate alias and its term.

``` unison
type alias Element = Nat
type alias Container = Element
identity : Container -> Container
identity n = n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Container = Element
  + type alias Element = Nat

  + identity : Container -> Container

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
type alias Element = Text
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Element = Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

> view Container

  type alias Container = Element

> view identity

  identity : Container -> Container
  identity n = n
```

``` unison
> identity "updated"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > identity "updated"
          ⧩
          "updated"
```

``` unison
type alias A = Nat
type alias B = A

increment : B -> B
increment n = n + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias A = Nat
  + type alias B = A

  + increment : B -> B

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Changing the underlying type must check the term through the updated
intermediate alias. It cannot silently leave the term on the old alias.

``` unison
type alias A = Text
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias A = Text

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
type alias A = Text

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

type alias B = A

increment : B -> B
increment n =
  use Nat +
  n + 1

```
