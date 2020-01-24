### Transcript parser operations

The transcript parser is meant to parse `ucm` and `unison` blocks.

```unison
x = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat

```
```unison
z
```

```ucm
.> delete foo

  ⚠️
  
  I don't know about that name.

```
However handling of blocks of other languages should be supported.

```
python
some python code

```

```
c_cpp
some C++ code

```

```
c9search
some cloud9 code

```

