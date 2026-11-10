Two independent layers of dependents. The connection layer: the record `Config` (the first layer), `Config.parse` that builds one with its constructor, `Server.connect` that uses `Config.parse`, and `Server.status` that uses `connect`. The retry layer: `Backoff.delay`, `Job.wait` that uses it, `Job.total` that uses `wait`, and `Job.within` that uses `total`. Nothing in one layer refers to the other. There is a test for each function.

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

Backoff.delay : Nat -> Nat
Backoff.delay attempt = 100 Nat.* Nat.pow 2 attempt

Job.wait : Nat -> Nat
Job.wait attempt = Backoff.delay attempt

Job.total : Nat -> Nat
Job.total attempts = List.foldLeft (acc n -> acc Nat.+ Job.wait n) 0 (List.range 0 attempts)

Job.within : Nat -> Nat -> Boolean
Job.within attempts limit = Job.total attempts Nat.<= limit

test> Config.parse.tests.example = test.verify do
  ensureEqual (Config.parse "example.com") (Config "example.com" 3)

test> Server.connect.tests.example = test.verify do
  ensureEqual (Server.connect "example.com") "connect example.com (retries 3)"

test> Server.status.tests.example = test.verify do
  ensureEqual (Server.status "example.com") "connect example.com (retries 3): ok"

test> Backoff.delay.tests.example = test.verify do
  ensureEqual (Backoff.delay 3) 800

test> Job.wait.tests.example = test.verify do
  ensureEqual (Job.wait 2) 400

test> Job.total.tests.example = test.verify do
  ensureEqual (Job.total 3) 700

test> Job.within.tests.example = test.verify do
  ensureEqual (Job.within 3 700, Job.within 3 699) (true, false)
````

``` ucm
scratch/main> add
scratch/main> test
```
