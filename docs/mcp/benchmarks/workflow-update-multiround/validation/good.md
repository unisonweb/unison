The worker's best case: one block that holds both changes and every definition and test that has to change, saved with one `update` (nothing is left outside the block, so it finishes at once).

```` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 port

Server.connect : Text -> Nat -> Text
Server.connect host port =
  c = Config.parse host port
  "connect " ++ Config.host c ++ ":" ++ Nat.toText (Config.port c) ++ " (retries " ++ Nat.toText (Config.retries c) ++ ")"

Server.status : Text -> Nat -> Text
Server.status host port = Server.connect host port ++ ": ok"

Backoff.delay : Nat -> Nat -> Nat
Backoff.delay base attempt = base Nat.* Nat.pow 2 attempt

Job.wait : Nat -> Nat -> Nat
Job.wait base attempt = Backoff.delay base attempt

Job.total : Nat -> Nat -> Nat
Job.total base attempts = List.foldLeft (acc n -> acc Nat.+ Job.wait base n) 0 (List.range 0 attempts)

Job.within : Nat -> Nat -> Nat -> Boolean
Job.within base attempts limit = Job.total base attempts Nat.<= limit

test> Config.parse.tests.example = test.verify do
  ensureEqual (Config.parse "example.com" 8080) (Config "example.com" 3 8080)

test> Server.connect.tests.example = test.verify do
  ensureEqual (Server.connect "example.com" 8080) "connect example.com:8080 (retries 3)"

test> Server.status.tests.example = test.verify do
  ensureEqual (Server.status "example.com" 8080) "connect example.com:8080 (retries 3): ok"

test> Backoff.delay.tests.example = test.verify do
  ensureEqual (Backoff.delay 100 3) 800

test> Job.wait.tests.example = test.verify do
  ensureEqual (Job.wait 50 2) 200

test> Job.total.tests.example = test.verify do
  ensureEqual (Job.total 100 3) 700

test> Job.within.tests.example = test.verify do
  ensureEqual (Job.within 100 3 700, Job.within 100 3 699) (true, false)
````

``` ucm
scratch/main> update
scratch/main> test
scratch/main> todo
scratch/main> branches
```
