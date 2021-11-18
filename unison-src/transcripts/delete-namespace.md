# delete.namespace.force

```ucm:hide
.> builtins.merge
```

```unison
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

```ucm:error
.> delete.namespace.force dependencies
```
