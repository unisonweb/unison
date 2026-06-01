# Aliases referencing aliases

An alias body may mention other aliases. Those references stay as alias
refs in the stored body — they aren't flattened at parse time.

``` ucm :hide
> builtins.mergeio
```

``` unison
type alias Endo a = a -> a

type alias Endo2 a = Endo (Endo a)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a
  + type alias Endo2 a = Endo (Endo a)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view Endo

  type alias Endo a = a -> a

> view Endo2

  type alias Endo2 a = Endo (Endo a)
```

`view Endo2` shows `Endo (Endo a)`, not the flattened
`(a -> a) -> (a -> a)`. `Endo2` is a dependent of `Endo` (its body
references `Endo` by hash).

Now redefine `Endo`. The change must propagate to `Endo2`: with its
upstream alias at a new hash, `Endo2` re-emits at a new hash too, with
its body substituted to point at the new `Endo`.

``` unison
type alias Endo a = Optional a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = Optional a

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view Endo

  type alias Endo a = Optional a

> view Endo2

  type alias Endo2 a = Endo (Endo a)
```
