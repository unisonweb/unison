You are judging the state of the Unison codebase you are connected to after a previous agent built a small web service in it. The project `scratch` started with only `@unison/base` installed. The agent was asked for `POST /sum`, taking a JSON body like `{"numbers": [1, 2, 3]}` and answering `{"total": 6}`, a 400 for a body that is not valid JSON of that shape, as a definition called `sumService`, with tests, and without leaving a server running. It had to find and install what it needed from Unison Share. You may use any tool except `learn`. Don't change the project's definitions, and don't depend on Unison Share being reachable. Never start a server: do not run `sumService` with `serveSimple`, `serve`, `run` or any `io.test` of your own that opens a port or waits for input.

Run each of the following and note the result of each:

1. `view sumService` and `ls lib`: `sumService` exists in the project (not in a library), and `lib` has `@unison/base` plus the libraries the agent installed (folders like `unison_routes_7_0_2`, `unison_json_...`, `unison_http_...`).
2. `dependencies sumService`: the service is built from library definitions that read a request and write a response, for example `decodeJson`, `bodyUtf8` or `Decoder.run`, and `ok.json` or `Json.object`; it is not only `Text` functions.
3. `find tests` or `ls sumService`: the project has tests of the service (names under `sumService.tests`, or similar), outside any library. `view` them: they exercise the service itself (through `harness` or by calling it with a request, or at least the decoding and encoding functions that `sumService` calls), they cover an ordinary list, and they cover a body that is rejected. Their expected values are written out.
4. `test` runs the project's tests, and every one passes. (Tests that need a port or the network do not run under `test`; do not start any. If the agent left such tests, say so, and judge the others.)
5. `branches` shows the project on `main` with no temporary branch (`update-*`, `merge-*`, `upgrade-*`), and `todo` reports nothing pending.
6. The definitions are in the codebase (found by `view`), not only in a scratch file.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
