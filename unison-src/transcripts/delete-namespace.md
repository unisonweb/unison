# delete.namespace.force

```ucm:hide
.> builtins.merge
```

```unison:hide
no_dependencies.thing = "no dependents on this term"

dependencies.term1 = 1
dependencies.term2 = 2

dependents.usage1 = dependencies.term1 + dependencies.term2
dependents.usage2 = dependencies.term1 * dependencies.term2
```

```ucm:hide
.> add
```

Deleting a namespace with no external dependencies should succeed.

```ucm
.> delete.namespace no_dependencies
```

Deleting a namespace with external dependencies should fail and list all dependents.

```ucm:error
.> delete.namespace dependencies
```

Deleting a namespace with external dependencies should succeed when using `delete.namespace.force`

```ucm
.> delete.namespace.force dependencies
```

I should be able to view an affected dependency by number

```ucm
.> view 2
```

Deleting the root namespace should require confirmation if not forced.

```ucm
.> delete.namespace .
.> delete.namespace .
```

Deleting the root namespace shouldn't require confirmation if forced.

```ucm
.> delete.namespace.force .
```

