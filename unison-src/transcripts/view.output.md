# View commands

```unison
a.thing = "a"
b.thing = "b"
```

```ucm
-- Should suffix-search and find values in sub-namespaces
scratch/main> view thing

  a.thing : Text
  a.thing = "a"
  
  b.thing : Text
  b.thing = "b"

-- Should be local to namespace
  ☝️  The namespace .a is empty.

.a> view thing

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    thing

```

```ucm
-- Should suffix-search and find values in sub-namespacesscratch/main> view thing-- Should be local to namespace.a> view thing-- view.global should search globally and be absolutely qualified.a> view.global thing-- Should support absolute paths outside of current namespace.a> view .b.thing
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    thing

