```` unison
ability Log where
  log : Text -> ()

Log.doc = {{
An ability for logging messages. {{ docExample 1 (do (msg -> Log.log msg)) }} records `msg`, and a handler decides what happens to the messages. See {Log.collect}.
}}

Log.collect.doc = {{
{{ docExample 1 (do (program -> Log.collect program)) }} runs `program`, a delayed computation that uses {Log.log}, and returns its result together with the list of messages it logged, in the order they were logged. Other abilities the computation uses are passed through. For example:

```
Log.collect do
  log "start"
  log "end"
  42
```

A computation that logs nothing gives an empty list:

```
Log.collect do 7
```

*Also see:* {type Log}
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
````
``` ucm
scratch/main> add
scratch/main> test
scratch/main> docs Log.collect
```
