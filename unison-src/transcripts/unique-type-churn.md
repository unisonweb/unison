This transcript demonstrates that unique types no longer always get a fresh GUID: they share GUIDs with already-saved
unique types of the same name.

```unison
unique type A = A

unique type B = B C
unique type C = C B
```

```ucm
.> add
```

```unison
unique type A = A

unique type B = B C
unique type C = C B
```

If the name stays the same, the churn is even prevented if the type is updated and then reverted to the original form.

```ucm
.> names A
```

```unison
unique type A = A ()
```

```ucm
.> update
.> names A
```

```unison
unique type A = A
```

Note that `A` is back to its original hash.

```ucm
.> update
.> names A
```
