```` unison
Log.line : Text -> Nat -> Text -> Text
Log.line level code message =
  use Text ++
  "[" ++ level ++ " " ++ Nat.toText code ++ "] " ++ message

Log.line.doc = {{
{{ docExample 3 (do (level code message -> Log.line level code message)) }} formats one log line from a
level, a numeric code and the message. For example:

@eval{ Log.line "WARN" 42 "disk low" }
}}

Log.levelFor : Nat -> Text
Log.levelFor code = if code >= 500 then "ERROR" else "WARN"

Log.startup : Text
Log.startup = Log.line "INFO" 1 "started"

Log.failure : Nat -> Text -> Text
Log.failure code detail =
  Log.line (Log.levelFor code) (code + 1000) ("failed: " ++ detail)

Log.errors : [Text] -> [Text]
Log.errors messages = List.map (Log.line "ERROR" 500) messages

Log.warn : Nat -> Text -> Text
Log.warn = Log.line "WARN"

Log.batch : [Text] -> [Text]
Log.batch messages = messages |> List.map (Log.line "INFO" 7)

Log.numbered : [Nat] -> [Text] -> [Text]
Log.numbered codes messages = List.zipWith (Log.line "NOTE") codes messages

Log.fatal : Text
Log.fatal = Log.line "FATAL" 9 <| "disk gone"

Log.bye : Text -> Text
Log.bye message = message |> Log.line "INFO" 0

Log.fromTriples : [(Text, Nat, Text)] -> [Text]
Log.fromTriples triples =
  List.map (cases (level, code, message) -> Log.line level code message) triples

Table.line : Text -> Nat -> Text
Table.line label width = label ++ Text.repeat width "."

Table.header : Text
Table.header = Table.line "Name" 8

Table.rows : [Text] -> [Text]
Table.rows labels = List.map (label -> Table.line label 3) labels

test> Log.tests.line = test.verify do
  ensureEqual (Log.line "WARN" 42 "disk low") "[WARN 42] disk low"
  ensureEqual Log.startup "[INFO 1] started"

test> Log.tests.failure = test.verify do
  ensureEqual (Log.failure 503 "timeout") "[ERROR 1503] failed: timeout"
  ensureEqual (Log.failure 404 "missing") "[WARN 1404] failed: missing"

test> Log.tests.partial = test.verify do
  ensureEqual (Log.errors ["a", "b"]) ["[ERROR 500] a", "[ERROR 500] b"]
  ensureEqual (Log.warn 3 "careful") "[WARN 3] careful"
  ensureEqual (Log.batch ["x"]) ["[INFO 7] x"]
  ensureEqual (Log.numbered [1, 2] ["p", "q"]) ["[NOTE 1] p", "[NOTE 2] q"]

test> Log.tests.piped = test.verify do
  ensureEqual Log.fatal "[FATAL 9] disk gone"
  ensureEqual (Log.bye "see you") "[INFO 0] see you"
  ensureEqual (Log.fromTriples [("DEBUG", 5, "x")]) ["[DEBUG 5] x"]

test> Table.tests.line = test.verify do
  ensureEqual Table.header "Name........"
  ensureEqual (Table.rows ["a", "b"]) ["a...", "b..."]
````

``` ucm
scratch/main> add
scratch/main> test
```
