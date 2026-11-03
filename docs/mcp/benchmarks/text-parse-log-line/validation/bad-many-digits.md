``` unison
type LogLine = { level : Text, hour : Nat, minute : Nat, message : Text }

LogLine.parse : Text -> Optional LogLine
LogLine.parse t =
  digits = Pattern.some Text.patterns.digit
  p = Pattern.join [Text.patterns.literal "[", Pattern.capture (Pattern.some (Text.patterns.notCharIn [?]])), Text.patterns.literal "] ", Pattern.capture digits, Text.patterns.literal ":", Pattern.capture digits, Text.patterns.literal " ", Pattern.capture (Pattern.many Text.patterns.anyChar), Text.patterns.eof]
  match Pattern.run p t with
    Some ([level, h, m, message], _) ->
      match (Nat.fromText h, Nat.fromText m) with
        (Some hour, Some minute) -> Some (LogLine level hour minute message)
        _ -> None
    _ -> None

test> LogLine.parse.tests.example = test.verify do
  ensureEqual (LogLine.parse "[ERROR] 12:30 disk full") (Some (LogLine "ERROR" 12 30 "disk full"))
  ensureEqual (LogLine.parse "[INFO] 07:05 started: ok [x]") (Some (LogLine "INFO" 7 5 "started: ok [x]"))
  ensureEqual (LogLine.parse "garbage") None
  ensureEqual (LogLine.parse "") None
  ensureEqual (LogLine.parse "[INFO] 12 no minutes") None
```
``` ucm
scratch/main> add
scratch/main> test
```
