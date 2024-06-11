```ucm:hide
scratch/main> alias.type ##Text builtin.Text
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
scratch/main> add
scratch/main> find : Text -> A
scratch/main> find : A -> Text
scratch/main> find : A
```
```ucm:error
scratch/main> find : Text
```
