```unison
test> foo : [Test.Result]
foo = []
```

Apparently when we add a test watch, we add a type annotation to it, even if it already has one.

```ucm
.> add

  ⍟ I've added these definitions:
  
    foo : [Result]

.> view foo

  foo : [Result]
  foo : [Result]
  foo = []

```
