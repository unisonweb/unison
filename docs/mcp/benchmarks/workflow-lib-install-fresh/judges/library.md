You are judging which library `parseNats` in the Unison codebase you are connected to is built on. A previous agent was asked to write `parseNats : Text -> Either Failure [Nat]` (JSON text that is an array of non-negative whole numbers into a list of `Nat`s) using an existing published library for the JSON, not a parser of its own. The project started with only `@unison/base` installed. You may use any tool except `learn`. Don't change the project's definitions. Checking a library's name may need the network (the Share tools).

Run each of the following, and note the result of each:

1. `list lib`: the folders under `lib`. `unison_base_7_20_0` was there from the start; anything else was installed by the agent. A folder of the form `unison_json_<version>` is `@unison/json`, the standard JSON library of the Unison ecosystem (`share-project-info` with `projectName` `@unison/json` shows it exists and its latest release).
2. `dependencies parseNats` and `view parseNats`: which library definitions it calls. The JSON work (parsing the text, checking it is an array, decoding the numbers) must be done by library definitions, not by `Text` functions that split on commas or look at characters.
3. `list lib` again to see whether the agent left unrelated libraries or two copies of the same library.

Rate:

- **GREAT**: `parseNats` is built on `@unison/json` (its `Decoder.run`, `Decoder.array` and `Decoder.nat`, or other definitions of that library) and `lib` holds `unison_json_<version>` once, with no stray libraries.
- **PASS**: `parseNats` is built on another published JSON library installed from Share (for example `@stew/json`, `@etorreborre/json-decoder` or `@mitchellwrosen/json-decode`, which show as another folder in `lib`) and it works (`> parseNats "[1, 2, 3]"` is `Right [1, 2, 3]`). Also PASS: `@unison/json` is used correctly but the agent also left an unrelated or duplicate library in `lib`.
- **FAIL**: no JSON library was installed, or one was installed but `parseNats` doesn't depend on it, or `parseNats` is a hand-written parser (splitting text, a lookup table for a few inputs).

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
