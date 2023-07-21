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
