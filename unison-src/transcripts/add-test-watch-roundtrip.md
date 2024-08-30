```ucm:hide
scratch/main> builtins.mergeio
```

```unison:hide
test> foo : [Test.Result]
foo = []
```

Apparently when we add a test watch, we add a type annotation to it, even if it already has one. We don't want this to happen though!

```ucm
scratch/main> add
scratch/main> view foo
```
