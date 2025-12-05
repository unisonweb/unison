These should error, this is because each term element is identical to one another except for the internal references.

We can't allow these terms into the codebase because in certain cases there are multiple valid distinct components which
would receive the same hash.

```unison :error
foo = do
  bar ()

bar = do
  foo ()
```
