# Update with conflicting ability constructor and term

```ucm:hide
.> builtins.merge
```

```unison
unique ability Stream where
  send : a -> ()

Stream.send : a -> ()
Stream.send _ = ()
```

```ucm
.> add
```
