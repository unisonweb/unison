```ucm:hide
.> alias.type ##Text builtin.Text
```

```unison:hide
unique type A = A Text

foo : A
foo = A "foo!"

bar : Text -> A
bar = A

baz : A -> Text
baz = cases
  A t -> t
```

```ucm
.> add
.> find : Text -> A
.> find : A -> Text
.> find : A
```
```ucm:error
.> find : Text
```
