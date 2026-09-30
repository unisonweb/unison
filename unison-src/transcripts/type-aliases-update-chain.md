# Updating aliases checks transitive dependents

```ucm:hide
> builtins.mergeio
```

A compatible update must move both the intermediate alias and its term.

```unison
type alias Element = Nat
type alias Container = Element
identity : Container -> Container
identity n = n
```

```ucm
> add
```

```unison
type alias Element = Text
```

```ucm
> update
> view Container
> view identity
```

```unison
> identity "updated"
```

```unison
type alias A = Nat
type alias B = A

increment : B -> B
increment n = n + 1
```

```ucm
> add
```

Changing the underlying type must check the term through the updated
intermediate alias. It cannot silently leave the term on the old alias.

```unison
type alias A = Text
```

```ucm:error
> update
```
