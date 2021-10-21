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
  ☝️  The namespace .example is empty.

.example> add

  ⍟ I've added these definitions:
  
    unique type A
    bar : Text -> A
    baz : A -> Text
    foo : A

.example> find : Text -> A

  1. bar : Text -> A
  2. A.A : Text -> A
  

.example> find : A -> Text

  1. baz : A -> Text
  

.example> find : A

  1. foo : A
  

```
```ucm
.example> find : Text

  ☝️
  
  I couldn't find exact type matches, resorting to fuzzy
  matching...

  1. bar : Text -> A
  2. baz : A -> Text
  3. A.A : Text -> A
  

```
