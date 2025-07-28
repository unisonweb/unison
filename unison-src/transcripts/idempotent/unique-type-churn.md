This transcript demonstrates that unique types no longer always get a fresh GUID: they share GUIDs with already-saved
unique types of the same name.

``` unison
unique type A = A

unique type B = B C
unique type C = C B
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type A
  + type B
  + type C

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
unique type A = A

unique type B = B C
unique type C = C B
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

If the name stays the same, the churn is even prevented if the type is updated and then reverted to the original form.

``` ucm
scratch/main> names A

  'A':
  Hash            Kind   Names
  #j743idicb1     Type   A
  #j743idicb1#0   Term   A.A
```

``` unison
unique type A = A ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type A

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> names A

  'A':
  Hash            Kind   Names
  #186m0i6upt     Type   A
  #186m0i6upt#0   Term   A.A
```

``` unison
unique type A = A
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type A

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

Note that `A` is back to its original hash.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> names A

  'A':
  Hash            Kind   Names
  #j743idicb1     Type   A
  #j743idicb1#0   Term   A.A
```
