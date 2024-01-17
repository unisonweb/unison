```unison
test> foo : [Test.Result]
foo = []
```

Apparently when we add a test watch, we add a type annotation to it, even if it already has one. We don't want this to happen though!

```ucm
.> add

  ⍟ I've added these definitions:
  
    foo : [Result]

.> view foo

  foo : [Result]
  foo : [Result]
  foo = []

```
