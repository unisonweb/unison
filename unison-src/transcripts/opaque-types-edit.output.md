# `edit` pulls an opaque type and its body fns back into the scratch file

`edit Logarithm` (or `edit Logarithm.fromFloat`) renders the full opaque
declaration into the latest scratch file so the user can iterate on it.
Mirrors `view` (see `opaque-types-view-edit.md`) but writes to a file
instead of the console.

``` ucm :hide
> builtins.mergeio
```

``` unison
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = Float.log x

  toFloat : Logarithm -> Float
  toFloat l = l
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + Logarithm.fromFloat : Float -> #jqtsjdc9s6
  + Logarithm.toFloat   : #jqtsjdc9s6 -> Float

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Done.

> edit Logarithm

  ☝️

  I added 1 definitions to the top of scratch.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
```

``` unison :added-by-ucm scratch.u
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = log x
  toFloat : Logarithm -> Float
  toFloat l = l
```

`edit` on a body fn name dispatches to the parent opaque type — same as
`view` (see `opaque-types-view-edit.md`).

``` ucm
> edit Logarithm.fromFloat

  ☝️

  I added 1 definitions to the top of scratch.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
```

``` unison :added-by-ucm scratch.u
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = log x
  toFloat : Logarithm -> Float
  toFloat l = l
```
