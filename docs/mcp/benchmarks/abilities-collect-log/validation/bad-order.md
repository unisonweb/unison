``` unison
ability Log where
  log : Text -> ()

Log.collect : '{g, Log} a ->{g} (a, [Text])
Log.collect program =
  h : [Text] -> Request {Log} a ->{g} (a, [Text])
  h acc = cases
    { a } -> (a, acc)
    { Log.log msg -> resume } -> handle resume() with h (msg +: acc)
  handle program() with h []

test> Log.collect.tests.example = test.verify do
  ensureEqual (Log.collect do 7) (7, [])
```
``` ucm
scratch/main> add
scratch/main> test
```
