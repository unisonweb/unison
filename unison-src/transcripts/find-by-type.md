```ucm:hide
.> alias.type ##Text builtin.Text
```

```unison
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
.example> add
.example> find : Text -> A
.example> find : A -> Text
.example> find : A
```
```ucm:error
.example> find : Text
```
