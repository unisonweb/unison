<!-- expected: no-collateral FAIL -->
<!-- A textual find and replace of `line` swapped `Table.line`, which only looks the same, and its callers. The behavior of
     everything is unchanged, but four definitions of the unrelated `Table` have been rewritten. -->

``` unison
Log.line : Nat -> Text -> Text -> Text
Log.line code level message =
  use Text ++
  "[" ++ level ++ " " ++ Nat.toText code ++ "] " ++ message

Log.batch : [Text] -> [Text]
Log.batch messages = messages |> List.map (Log.line 7 "INFO")

Log.bye : Text -> Text
Log.bye message = message |> Log.line 0 "INFO"

Log.errors : [Text] -> [Text]
Log.errors messages = List.map (Log.line 500 "ERROR") messages

Log.failure : Nat -> Text -> Text
Log.failure code detail =
  use Nat +
  use Text ++
  Log.line (code + 1000) (levelFor code) ("failed: " ++ detail)

Log.fatal : Text
Log.fatal = Log.line 9 "FATAL" <| "disk gone"

Log.fromTriples : [(Text, Nat, Text)] -> [Text]
Log.fromTriples triples =
  List.map
    (cases (level, code, message) -> Log.line code level message)
    triples

Log.startup : Text
Log.startup = Log.line 1 "INFO" "started"

Log.line.doc : Doc
Log.line.doc =
  {{
  {{ docExample 3 do code level message -> Log.line code level message }}
  formats one log line from a numeric code, a level and the message. For
  example:
  
  @eval{ Log.line 42 "WARN" "disk low" }
  }}

test> Log.tests.line = test.verify do
  ensureEqual (Log.line 42 "WARN" "disk low") "[WARN 42] disk low"
  ensureEqual Log.startup "[INFO 1] started"

Log.numbered : [Nat] -> [Text] -> [Text]
Log.numbered codes messages =
  List.zipWith (code message -> Log.line code "NOTE" message) codes messages

Log.warn : Nat -> Text -> Text
Log.warn code message = Log.line code "WARN" message

Table.line : Nat -> Text -> Text
Table.line width label = label ++ Text.repeat width "."

Table.header : Text
Table.header = Table.line 8 "Name"

Table.rows : [Text] -> [Text]
Table.rows labels = List.map (label -> Table.line 3 label) labels

test> Table.tests.line = test.verify do
  ensureEqual Table.header "Name........"
  ensureEqual (Table.rows ["a", "b"]) ["a...", "b..."]

```

``` ucm
scratch/main> update
scratch/main> test
```
