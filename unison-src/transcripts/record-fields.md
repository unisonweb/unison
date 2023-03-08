```ucm:hide
.> builtins.merge
```

Define a simple record type:

```unison
unique type User = {
  id : Nat,
  name : Text
}
```

```ucm:hide
.> add
```

It should now pretty-print as a record:

```ucm
.> view User
```

But the `modify` function on record fields is changed with a pretty-printer roundtrip. This shouldn't prompt me to update:

```ucm
.> edit User.id.modify
.> load scratch.u
```

If I go ahead and do the update, then my type is no longer treated as a record:

```ucm
.> update
.> view User
```
