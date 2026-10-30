You are judging the behavior of the `Config`, `Server`, `Backoff` and `Job` functions in the Unison codebase you are connected to. A previous agent added a `port : Nat` field (last) to `Config` and made `Config.parse`, `Server.connect` and `Server.status` take the port as one more, last, `Nat` argument; `Config.parse` still sets `retries` to `3`. It also gave `Backoff.delay`, `Job.wait`, `Job.total` and `Job.within` the base delay as a new first `Nat` argument: `Backoff.delay base attempt` is `base * 2^attempt` (it used to be `100 * 2^attempt`), `Job.wait base attempt` is that delay, `Job.total base attempts` is the sum of `Job.wait base n` for `n` from `0` up to but not including `attempts`, and `Job.within base attempts limit` is whether that total is at most `limit`. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Evaluate each of these in a `unison` block of watch expressions (`> Server.connect "example.com" 443` and so on) and compare with the expected value. Use ports and bases other than 8080 and 100, because an implementation could have the number written into it.

1. `Config.port (Config.parse "x.org" 9)` is `9`, `Config.host (Config.parse "x.org" 9)` is `"x.org"`, `Config.retries (Config.parse "x.org" 9)` is `3`.
2. `Server.connect "example.com" 443` is `"connect example.com:443 (retries 3)"`, and `Server.connect "example.com" 8080` is `"connect example.com:8080 (retries 3)"`.
3. `Server.status "example.com" 443` is `"connect example.com:443 (retries 3): ok"`.
4. `Backoff.delay 10 3` is `80`, `Backoff.delay 100 3` is `800`, `Backoff.delay 7 0` is `7`.
5. `Job.wait 50 2` is `200`.
6. `Job.total 10 3` is `70` (10 + 20 + 40), `Job.total 100 3` is `700`, `Job.total 5 0` is `0`.
7. `(Job.within 10 3 70, Job.within 10 3 69)` is `(true, false)`.

It passes only if every value matches.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
