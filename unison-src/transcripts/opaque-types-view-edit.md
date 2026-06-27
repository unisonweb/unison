# view dispatches body-fn queries to the parent opaque type

After adding an opaque type, `view <OpaqueName>` and `view <OpaqueName>.<body-fn>`
both render the full opaque decl. This is the namespace-pattern membership
lookup from plan §2.2.

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
> view Logarithm
```

`view` on the parent name renders the whole block, including the body fns.

```ucm
> view Logarithm.fromFloat
```

`view` on a body fn name dispatches to the parent opaque type — same rendered
block.

```ucm
> view Logarithm.toFloat
```

Same result regardless of which body fn was named.
