A small client library over four layers: the record `Config` (the first layer), `Config.parse` that builds one with its constructor, `Server.connect` that uses `Config.parse`, `Server.status` that uses `connect`, and `Server.report` that uses `status`, with a test for each function.

```` unison
unique type Config = { host : Text, retries : Nat }

Config.parse : Text -> Config
Config.parse host = Config host 3

Server.connect : Text -> Text
Server.connect host =
  c = Config.parse host
  "connect " ++ Config.host c ++ " (retries " ++ Nat.toText (Config.retries c) ++ ")"

Server.status : Text -> Text
Server.status host = Server.connect host ++ ": ok"

Server.report : [Text] -> Text
Server.report hosts = Text.join "\n" (List.map Server.status hosts)

test> Config.parse.tests.example = test.verify do
  ensureEqual (Config.parse "example.com") (Config "example.com" 3)

test> Server.connect.tests.example = test.verify do
  ensureEqual (Server.connect "example.com") "connect example.com (retries 3)"

test> Server.status.tests.example = test.verify do
  ensureEqual (Server.status "example.com") "connect example.com (retries 3): ok"

test> Server.report.tests.example = test.verify do
  ensureEqual (Server.report ["a.com", "b.org"]) "connect a.com (retries 3): ok\nconnect b.org (retries 3): ok"
````

``` ucm
scratch/main> add
scratch/main> test
```
