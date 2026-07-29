# `run>` watches and `IO`

`run>` watches accept a `'{IO, Exception} a` thunk and force it, so unlike a
regular `>` watch they can actually perform `IO`.

## It runs `IO`-requiring code

Here we write to a temp file and read back the result:

``` unison
run> do autoCleaned do catch do
  file = newTempDir "run-watch" ++ "/message.txt"
  writeFile file (toUtf8 "Hello!")
  fromUtf8 (readFile file)
```

## A raised exception is reported nicely

If the forced thunk raises an `Exception`, it's reported as an evaluation
failure rather than crashing:

``` unison :error
run> do Exception.raise (Exception.failure "boom" ())
```
