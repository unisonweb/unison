``` unison
ability Log where
  log : Text -> ()

Log.collect.doc = {{
{{ docExample 1 (do (program -> Log.collect program)) }} runs `program` and returns a pair of the result and the messages it logged, most recent message first.
}}

Log.collect : '{g, Log} a ->{g} (a, [Text])
Log.collect program =
  h : [Text] -> Request {Log} a ->{g} (a, [Text])
  h acc = cases
    { a } -> (a, acc)
    { Log.log msg -> resume } -> handle resume() with h (acc :+ msg)
  handle program() with h []

test> Log.collect.tests.example = test.verify do
  ensureEqual (Log.collect do log "start"; log "end"; 42) (42, ["start", "end"])
  ensureEqual (Log.collect do 7) (7, [])
  r = Store.withInitialValue 0 do
    Log.collect do
      log "a"
      Store.modify (n -> n + 1)
      log "b"
      Store.get
  ensureEqual r (1, ["a", "b"])
```
``` ucm
scratch/main> add
scratch/main> test
```
