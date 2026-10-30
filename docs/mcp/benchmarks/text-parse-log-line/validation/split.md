``` unison
type LogLine = { level : Text, hour : Nat, minute : Nat, message : Text }

LogLine.parse : Text -> Optional LogLine
LogLine.parse t =
  if not (Text.startsWith "[" t) then None
  else match Text.splitOnText "] " (Text.drop 1 t) with
    level +: restPieces ->
      rest = Text.join "] " restPieces
      match Text.splitOnText " " rest with
        time +: msgPieces ->
          message = Text.join " " msgPieces
          match Text.splitOnText ":" time with
            [h, m] -> match (Nat.fromText h, Nat.fromText m) with
              (Some hour, Some minute) -> Some (LogLine level hour minute message)
              _ -> None
            _ -> None
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
