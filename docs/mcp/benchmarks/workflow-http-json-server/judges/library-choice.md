You are judging which libraries the service `sumService` was built from in the Unison codebase you are connected to. The project `scratch` started with only `@unison/base`. A previous agent was asked, without being told any library name, for a web service: `POST /sum` takes a JSON body like `{"numbers": [1, 2, 3]}` and answers `{"total": 6}`. It had to find libraries on Unison Share. You may use any tool except `learn`. Don't change the project's definitions. Don't depend on Unison Share being reachable.

Run `ls lib`, `view sumService`, and `dependencies sumService`; use `view` on helpers `sumService` calls in the project, and `dependencies` on them, to see everything that is built from where. A library's definitions have names starting `lib.<folder>.`; a library can also contain its own `lib` (for example `@unison/routes` nests `unison_json_` and `unison_http_` copies), and that counts as using that library.

Background: on Unison Share, `@unison/routes` is the library for HTTP routes (`Route` ability, `route`, `noCapture`, `decodeJson`, `ok.json`, `harness` for tests) and builds on `@unison/http`; `@unison/json` is the JSON library (`Decoder`, `Json`). `@unison/httpserver` is archived (its README says it moved into `@unison/http`).

Rate:

- **GREAT**: the service uses `@unison/routes` for the HTTP side (a `Route` computation) and `@unison/json` for the JSON, either installed in `lib` next to it or the copy nested inside `lib.unison_routes_...` that `routes` itself uses (using the nested copy avoids a second, incompatible `Json` type and is as good as installing it): the request body is decoded with `decodeJson` or `Decoder.run` and the answer is built with `Json.object` and `ok.json`, not as hand-assembled text.
- **PASS**: a different combination that reasonably does the job: `@unison/http` directly for the server together with `@unison/json`; or `@unison/routes` with another JSON library installed from Share (for example `@stew/json` or the `@etorreborre` JSON libraries), as long as the JSON is decoded and encoded by a library and the service works.
- **FAIL**: any of these: the JSON body is read or the JSON answer is built by hand (`Text.split`, `Text.drop`, `Nat.fromText` on pieces of the body, `++` to assemble the answer text), even when a library handles the HTTP side; `sumService` is built on `@unison/httpserver` (`lib` has `unison_httpserver_...` and `sumService` depends on it) or on another library that Share marks as archived or private; or `sumService` does not exist or has no HTTP or JSON library in its dependencies at all.

An installed but unused library does not count in either direction. Judge the service itself, not whether it passes tests (other judges do that).

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
