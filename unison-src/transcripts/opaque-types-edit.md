# `edit` pulls an opaque type and its body fns back into the scratch file

`edit Logarithm` (or `edit Logarithm.fromFloat`) renders the full opaque
declaration into the latest scratch file so the user can iterate on it.
Mirrors `view` (see `opaque-types-view-edit.md`) but writes to a file
instead of the console.

```ucm :hide
> builtins.mergeio
```

```unison
opaque type Logarithm = Float where
  fromFloat : Float -> Logarithm
  fromFloat x = Float.log x

  toFloat : Logarithm -> Float
  toFloat l = l
```

```ucm
> add
> edit Logarithm
```

`edit` on a body fn name dispatches to the parent opaque type — same as
`view` (see `opaque-types-view-edit.md`).

```ucm
> edit Logarithm.fromFloat
```
