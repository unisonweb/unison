# delete.namespace.force

```unison
no_dependencies.thing = "no dependents on this term"

dependencies.term1 = 1
dependencies.term2 = 2

dependents.usage1 = dependencies.term1 + dependencies.term2
dependents.usage2 = dependencies.term1 * dependencies.term2
```

Deleting a namespace with no external dependencies should succeed.

```ucm
scratch/main> delete.namespace no_dependencies

  Done.

```
Deleting a namespace with external dependencies should fail and list all dependents.

```ucm
scratch/main> delete.namespace dependencies

  ⚠️
  
  I didn't delete the namespace because the following
  definitions are still in use.
  
  Dependency   Referenced In
  term2        1. dependents.usage1
               2. dependents.usage2
               
  term1        3. dependents.usage1
               4. dependents.usage2
  
  If you want to proceed anyways and leave those definitions
  without names, use delete.namespace.force

```
Deleting a namespace with external dependencies should succeed when using `delete.namespace.force`

```ucm
scratch/main> delete.namespace.force dependencies

  Done.

  ⚠️
  
  Of the things I deleted, the following are still used in the
  following definitions. They now contain un-named references.
  
  Dependency   Referenced In
  term2        1. dependents.usage1
               2. dependents.usage2
               
  term1        3. dependents.usage1
               4. dependents.usage2

```
I should be able to view an affected dependency by number

```ucm
scratch/main> view 2

  dependents.usage2 : Nat
  dependents.usage2 =
    use Nat *
    #gjmq673r1v * #dcgdua2lj6

```
Deleting the root namespace should require confirmation if not forced.

```ucm
scratch/main> delete.namespace .

```

```ucm
scratch/main> delete.namespace .scratch/main> delete.namespace .-- Should have an empty historyscratch/main> history .
```


🛑

The transcript failed due to an error in the stanza above. The error is:

1:1:
  |
1 | .
  | ^
unexpected '.'
expecting '`' or operator (valid characters: !$%&*+-/:<=>\^|~)

