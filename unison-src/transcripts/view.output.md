# View commands

```unison
a.thing = "a"
b.thing = "b"
```

```ucm
-- Should suffix-search and find values in sub-namespaces
.> view thing

  a.thing : Text
  a.thing = "a"
  
  b.thing : Text
  b.thing = "b"

-- Should be local to namespace
.a> view thing

  thing : Text
  thing = "a"

-- view.global should search globally and be absolutely qualified
.a> view.global thing

  .a.thing : Text
  .a.thing = "a"
  
  .b.thing : Text
  .b.thing = "b"

-- Should support absolute paths outside of current namespace
.a> view .b.thing

  .b.thing : Text
  .b.thing = "b"

```
