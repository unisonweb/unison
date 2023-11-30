Merge builtins so we get enough names for the testing stuff.

The `test` command should run all of the tests in the current directory.

(Just so we don't have to pull in `.base` into this transcript, we make a fakey test just by giving it the right type,
and manually linking it to the builtin `isTest` value).

```unison
test1 : [Result]
test1 = [Ok "test1"]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test1 : [Result]

```
```ucm
.> add.> link .builtin.metadata.isTest test1
```


🛑

The transcript failed due to an error in the stanza above. The error is:

⚠️
I don't know how to link. Type `help` or `?` to get help.
