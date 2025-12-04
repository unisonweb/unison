```ucm
scratch/main> builtins.merge
```


This creates a cycle of structurally equivalent elements, which have an ambiguous ordering.

On top level components this is an error, but we don't want to error on letrecs which are internal to a definition.

See `unison-src/transcripts/errors/incomplete-element-ordering.md` for
examples which should trigger failures.

```unison
foo =
  x = do 1 + y()
  y = do 1 + x()
  x ()
```
