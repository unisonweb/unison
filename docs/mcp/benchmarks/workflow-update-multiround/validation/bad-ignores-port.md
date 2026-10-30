<!-- expected: behavior FAIL, outcome PASS -->
`Config.parse` ignores its port argument and always stores 8080, which the tests do not notice because they all use 8080 (the report test was changed from port 80 to 8080 as well).

```` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 8080

Server.connect : Text -> Nat -> Text
Server.connect host port =
  c = Config.parse host port
  "connect " ++ Config.host c ++ ":" ++ Nat.toText (Config.port c) ++ " (retries " ++ Nat.toText (Config.retries c) ++ ")"

Server.status : Text -> Nat -> Text
Server.status host port = Server.connect host port ++ ": ok"

Server.report : [Text] -> Nat -> Text
Server.report hosts port = Text.join "\n" (List.map (host -> Server.status host port) hosts)

test> Config.parse.tests.example = test.verify do
  ensureEqual (Config.parse "example.com" 8080) (Config "example.com" 3 8080)

test> Server.connect.tests.example = test.verify do
  ensureEqual (Server.connect "example.com" 8080) "connect example.com:8080 (retries 3)"

test> Server.status.tests.example = test.verify do
  ensureEqual (Server.status "example.com" 8080) "connect example.com:8080 (retries 3): ok"

test> Server.report.tests.example = test.verify do
  ensureEqual (Server.report ["a.com", "b.org"] 8080) "connect a.com:8080 (retries 3): ok\nconnect b.org:8080 (retries 3): ok"
````

``` ucm
scratch/main> update
scratch/main> test
scratch/main> todo
scratch/main> branches
```
