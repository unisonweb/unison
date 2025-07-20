``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
test> foo : [Test.Result]
foo = []
```

Apparently when we add a test watch, we add a type annotation to it, even if it already has one. We don't want this to happen though\!

``` ucm
scratch/main> add
  Okay, I'm searching the branch for code that needs to be
  updated...
  Done.
scratch/main> view foo
  foo : [Result]
  foo : [Result]
  foo = []
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
This branch has more than one term with the name
`builtin.ImmutableByteArray.fromBytes`. Please delete or rename
all but one of them, then try the update again.
```
