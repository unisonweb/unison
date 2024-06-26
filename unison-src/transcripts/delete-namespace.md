# delete.namespace.force

```ucm:hide
scratch/main> builtins.merge
```

```unison:hide
no_dependencies.thing = "no dependents on this term"

dependencies.term1 = 1
dependencies.term2 = 2

dependents.usage1 = dependencies.term1 + dependencies.term2
dependents.usage2 = dependencies.term1 * dependencies.term2
```

```ucm:hide
scratch/main> add
```

Deleting a namespace with no external dependencies should succeed.

```ucm
scratch/main> delete.namespace no_dependencies
```

Deleting a namespace with external dependencies should fail and list all dependents.

```ucm:error
scratch/main> delete.namespace dependencies
```

Deleting a namespace with external dependencies should succeed when using `delete.namespace.force`

```ucm
scratch/main> delete.namespace.force dependencies
```

I should be able to view an affected dependency by number

```ucm
scratch/main> view 2
```

Deleting the root namespace should require confirmation if not forced.

```ucm
scratch/main> delete.namespace .
scratch/main> delete.namespace .
-- Should have an empty history
scratch/main> history .
```

Deleting the root namespace shouldn't require confirmation if forced.

```ucm
scratch/main> delete.namespace.force .
-- Should have an empty history
scratch/main> history .
```

