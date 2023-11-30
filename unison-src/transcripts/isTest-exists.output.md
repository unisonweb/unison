This transcript tests that UCM can always access the definition of
`IsTest`/`isTest`, which is used internally.

```ucm
.> builtins.merge

  Done.

```
```unison
test> pass = [Ok "Passed"]
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    pass : [Result]

.> links pass

```

```ucm
.> add.> links pass.> display 1
```


🛑

The transcript failed due to an error in the stanza above. The error is:

⚠️
I don't know how to links. Type `help` or `?` to get help.
