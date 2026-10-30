``` unison
ability Log where
  log : Text -> ()

Log.collect : '{Log} a -> (a, [Text])
Log.collect program =
  h : [Text] -> Request {Log} a -> (a, [Text])
  h acc = cases
    { a } -> (a, acc)
    { Log.log msg -> resume } -> handle resume() with h (acc :+ msg)
  handle program() with h []

test> Log.collect.tests.example = test.verify do
  ensureEqual (Log.collect do log "start"; log "end"; 42) (42, ["start", "end"])
  ensureEqual (Log.collect do 7) (7, [])
```
``` ucm
scratch/main> add
scratch/main> test
```
