You are judging which way `Person.toJson` and `Person.fromJson` were built in the Unison codebase you are connected to. The project `scratch` started with only `@unison/base` and a `Person` record. A previous agent had to write functions that turn a `Person` into JSON text and back, and was free to look for a library on Unison Share. You may use any tool except `learn`. Don't change the project's definitions. Don't depend on Unison Share being reachable (you may use the Share tools to look something up, but nothing below needs them).

Run `list lib`, `view Person.toJson`, `view Person.fromJson`, and `dependencies Person.toJson` and `dependencies Person.fromJson`.

Rate:

- **GREAT**: `list lib` has `@unison/json` (a folder named `unison_json_` followed by a version; the version does not matter) and both functions are built from it: `toJson` builds a `Json` value (`Json.object`, `Json.text`, `Json.nat`, ...) and prints it with `Json.toText`, and `fromJson` uses `Decoder` (`Decoder.run`, `Decoder.object.at!`, `Decoder.text`, `Decoder.nat`, ...) or `Json.fromText` together with a match on the `Json`, with failures turned into `Left` (for example by `catch`). `dependencies` shows definitions under `lib.unison_json_...`.
- **PASS**: the functions work (the `behavior` judge's job; don't rerun it) and use another JSON library that was installed from Share into `lib` (for example `@stew/json`, `@etorreborre/json-decoder` with `@etorreborre/json-encoder`, `@mitchellwrosen/json-decode`), with no JSON text built by hand. Also PASS: `@unison/json` is used for one function and the other is the same library's counterpart through another path, as long as no text is assembled by hand.
- **FAIL**: either function builds or reads the JSON text by hand, with `++`, `Text.join`, `Text.split`, `Text.toCharList`, `Pattern` or similar; or no JSON library appears in `dependencies`; or the functions do not exist.

An installed but unused library is not enough, and a hand-written parser inside the project is a FAIL even if it works.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
