``` ucm :hide
> alias.type ##Text builtin.Text
```

``` unison :hide
unique type A = A Text

foo : A
foo = A "foo!"

bar : Text -> A
bar = A

baz : A -> Text
baz = cases
  A t -> t
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> find : Text -> A

  1. bar : Text -> A
  2. A.A : Text -> A

> find : A -> Text

  1. baz : A -> Text

> find : A

  1. foo : A
```

``` ucm :error
> find : Text

  ☝️

  I couldn't find exact type matches, resorting to fuzzy
  matching...

  1. baz : A -> Text
  2. bar : Text -> A
  3. A.A : Text -> A
```
