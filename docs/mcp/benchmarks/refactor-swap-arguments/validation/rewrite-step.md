<!-- Not a solution: step 1 of validation/good.md, kept apart because a `unison` block after `rewrite` is ignored by the
     transcript runner (the rewritten file goes into a second transcript). It changes nothing in the codebase. One rule,
     `Log.line level code ==> Log.lineSwapped code level`, rewrites every use that has at least two arguments (full
     calls, `|>`, `<|`, and the partial application `Log.line "ERROR" 500` passed to `List.map`) and the doc example.
     It leaves alone `Log.warn` and `Log.numbered`, which pass only the first argument, and `Table.line`. -->

``` unison
Log.lineSwapped : Nat -> Text -> Text -> Text
Log.lineSwapped code level message =
  use Text ++
  "[" ++ level ++ " " ++ Nat.toText code ++ "] " ++ message

swap level code = @rewrite term Log.line level code ==> Log.lineSwapped code level

Log.batch : [Text] -> [Text]
Log.batch messages = messages |> List.map (Log.line "INFO" 7)

Log.bye : Text -> Text
Log.bye message = message |> Log.line "INFO" 0

Log.errors : [Text] -> [Text]
Log.errors messages = List.map (Log.line "ERROR" 500) messages

Log.failure : Nat -> Text -> Text
Log.failure code detail =
  use Nat +
  use Text ++
  Log.line (levelFor code) (code + 1000) ("failed: " ++ detail)

Log.fatal : Text
Log.fatal = Log.line "FATAL" 9 <| "disk gone"

Log.fromTriples : [(Text, Nat, Text)] -> [Text]
Log.fromTriples triples =
  List.map
    (cases (level, code, message) -> Log.line level code message) triples

Log.startup : Text
Log.startup = Log.line "INFO" 1 "started"

Log.line.doc : Doc
Log.line.doc =
  use Log line
  {{
  {{ docExample 3 do level code message -> line level code message }} formats
  one log line from a level, a numeric code and the message. For example:
  
  @eval{ line "WARN" 42 "disk low" }
  }}

test> Log.tests.line = test.verify do
  ensureEqual (Log.line "WARN" 42 "disk low") "[WARN 42] disk low"
  ensureEqual startup "[INFO 1] started"
```

``` ucm
scratch/main> rewrite swap
```
