``` unison
type LogLine = { level : Text, hour : Nat, minute : Nat, message : Text }

LogLine.parse : Text -> Optional LogLine
LogLine.parse t =
  literal = Text.patterns.literal
  digit = Text.patterns.digit
  level = Pattern.capture (Pattern.some (Text.patterns.notCharIn [?]]))
  twoDigits = Pattern.capture (Pattern.join [digit, digit])
  message = Pattern.capture (Pattern.many Text.patterns.anyChar)
  line =
    Pattern.join
      [ literal "["
      , level
      , literal "] "
      , twoDigits
      , literal ":"
      , twoDigits
      , literal " "
      , message
      , Text.patterns.eof
      ]
  match Pattern.run line t with
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
  ensureEqual (LogLine.parse "[INFO] 123:45 too many hour digits") None
  ensureEqual (LogLine.parse "[INFO] 7:05 one hour digit") None
```
``` ucm
scratch/main> add
scratch/main> test
```
