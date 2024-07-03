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

-- view.global should search globally and be absolutely qualified
scratch/other> view.global thing

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    thing

```

```ucm
-- Should suffix-search and find values in sub-namespacesscratch/main> view thing-- view.global should search globally and be absolutely qualifiedscratch/other> view.global thing-- Should support absolute pathsscratch/main> view .b.thing-- Should support branch relative pathsscratch/other> view /main:.a.thing
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    thing

