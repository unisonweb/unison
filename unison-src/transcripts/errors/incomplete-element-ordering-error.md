These should error.

```unison :error
foo = do
  bar ()

bar = do
  foo ()
```
