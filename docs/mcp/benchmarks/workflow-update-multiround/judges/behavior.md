You are judging the behavior of the `Config` and `Server` functions in the Unison codebase you are connected to. A previous agent added a `port : Nat` field (last) to `Config` and made `Config.parse`, `Server.connect`, `Server.status` and `Server.report` take the port as one more, last, `Nat` argument. `Config.parse` still sets `retries` to `3`. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Evaluate each of these in a `unison` block of watch expressions (`> Server.connect "example.com" 443` and so on) and compare with the expected value. Use ports other than 8080, because an implementation could have the number written into it.

1. `Config.port (Config.parse "x.org" 9)` is `9`, `Config.host (Config.parse "x.org" 9)` is `"x.org"`, `Config.retries (Config.parse "x.org" 9)` is `3`.
2. `Server.connect "example.com" 443` is `"connect example.com:443 (retries 3)"`, and `Server.connect "example.com" 8080` is `"connect example.com:8080 (retries 3)"`.
3. `Server.status "example.com" 443` is `"connect example.com:443 (retries 3): ok"`.
4. `Server.report ["a.com", "b.org"] 80` is the two lines `connect a.com:80 (retries 3): ok` and `connect b.org:80 (retries 3): ok` joined by a newline (`"connect a.com:80 (retries 3): ok\nconnect b.org:80 (retries 3): ok"`), and `Server.report [] 80` is `""`.

It passes only if every value matches.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
