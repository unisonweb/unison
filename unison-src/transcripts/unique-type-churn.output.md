This transcript demonstrates that unique types no longer always get a fresh GUID: they share GUIDs with already-saved
unique types of the same name.

```unison
unique type A = A

unique type B = B C
unique type C = C B
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type A
      unique type B
      unique type C

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type A
    unique type B
    unique type C

```
```unison
unique type A = A

unique type B = B C
unique type C = C B
```

```ucm

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

```
