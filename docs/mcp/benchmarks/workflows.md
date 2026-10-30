# Workflow benchmark ideas

A list of ideas, nothing built yet. The tasks so far (see `ideas.md`) each test one function. These test whole UCM workflows: merging, updating, installing and upgrading libraries, picking libraries from Share. Conventions are the ones in `README.md`: a context-free worker gets `prompt.md` and the MCP server; judges then inspect a copy of the codebase it left. `prepare.sh` gives project `scratch`, branch `main`, `@unison/base` 7.20.0 in `lib`, then runs the task's `setup.md`.

Ratings: **easy / medium / hard to automate** refers to building the setup and the judges.

## Setup tricks (all tried with the built binary unless marked)

**Any transcript step that is expected to fail needs `:error`.** A stanza that hits a conflict stops the transcript with a `🛑` unless it is written as ` ```ucm :error `. Verified for `merge`, `update` and `upgrade` that cannot finish. `prepare.sh` only greps the output for `error occurred|🛑`, and an expected-error stanza does not print either.

**Two diverging branches.** `branch` switches to the new branch, and the prompt prefix of each command selects the branch it runs on, so you can interleave freely:

````
``` ucm
scratch/main> branch alice
scratch/main> branch bob
```
``` unison
foo : Nat
foo = 3
```
``` ucm
scratch/bob> update
```
````

(then a `unison` block and `scratch/alice> update` for the other side). `merge /bob` from `scratch/alice` then merges cleanly if the edits touch different definitions (verified: `I merged scratch/bob into scratch/alice`, `history` shows a merge commit with two parents).

**A deterministic merge conflict.** Both branches change the same definition from the common ancestor (`foo = 1` to `foo = 2` on alice and to `foo = 3` on bob). `scratch/alice> merge /bob` as a `:error` stanza leaves the temporary branch `merge-bob-into-alice`, a scratch file with the three-way markers (in the order alice, base, bob, as the guide says) and the unconflicted dependents (`bar = foo + 1`) listed under a comment. Verified.

**Gotcha: `UCM_MERGETOOL`.** If it is set in the environment (on Paul's machine it is `cursor --merge ...`), a conflicting `merge` launches that tool. Run setup and the worker's UCM with `UCM_MERGETOOL=` (empty works) so benchmarks do not open an editor. Worth putting in `prepare.sh`.

**The conflict file is not on disk after a transcript.** In the transcript the merge or update file is printed as a `unison :added-by-ucm` block under a temp directory path, and its name contains a hash (`update-main-522bf9d5.u`, `merge-bob-into-alice-368151a8.u`), so setup cannot refer to it. Prefer a starting state just before the risky command (the worker runs `merge`, `update`, `upgrade` itself). Leaving the codebase on the temporary branch is possible (the branch is saved, `branches` lists `update-main` / `merge-bob-into-alice`) but I did not check what the MCP server shows the worker for the file, so I would not rely on it.

**A type-changing update that conflicts.** Add `unique type Foo = Foo Nat` and `incrFoo : Foo -> Foo` to `main`, then `update` a block with `unique type Foo = Foo Nat Nat` as a `:error` stanza: UCM says `I couldn't complete the update, because some existing definitions would no longer typecheck`, puts you on `update-main`, and the file holds the new `Foo` and `incrFoo` under `The definitions below no longer typecheck`. Verified. For a worker-driven version, only typecheck the block in setup (a `unison` block without `update`), or put it in the prompt.

**Pinning a library version.** `lib.install @unison/base/releases/7.19.0` installs into `lib.unison_base_7_19_0`, and several versions can sit side by side (verified; the network works, 15 to 25 seconds for base and json). Pin everything in `setup.md` so Share releases appearing later change nothing.

**Offline library versions, with a real conflict.** `lib.install` only takes Share names, but a local project can stand in for a library with `fork <project>/<branch>: lib.<name>_<version>` (the colon after the branch is required). Recipe, verified end to end:

````
``` ucm
scratch/main> builtins.merge lib.builtins
scratch/main> project.create-empty mylib
mylib/main> builtins.merge lib.builtins
mylib/main> branch v1
```
(unison block: type Opt = Opt Nat, greet : Text -> Text, cfg : Opt; then mylib/v1> add)
``` ucm
mylib/v1> branch /v2
```
(unison block: Opt = Opt Nat Text, greet takes two Texts, cfg changed; then mylib/v2> update)
``` ucm
scratch/main> fork mylib/v1: lib.mylib_1_0_0
scratch/main> fork mylib/v2: lib.mylib_2_0_0
```
(unison block in scratch: code using lib.mylib_1_0_0.greet and the Opt constructor; scratch/main> add)
``` ucm :error
scratch/main> upgrade mylib_1_0_0 mylib_2_0_0
```
````

With `@unison/base` in `lib` instead of `builtins.merge`, use base names (`Nat`, `++`) as usual. The result is `I couldn't automatically upgrade ...`, temp branch `upgrade-mylib_1_0_0-to-mylib_2_0_0`. Fully offline, fully deterministic; the library has no Share identity, which a task that needs to search Share cannot use. Two things I hit on the way: a constructor is `lib.mylib_1_0_0.Opt.Opt`, and a fresh `project.create-empty` has no builtins, hence `builtins.merge`.

**A real clean upgrade pair (needs network).** `@unison/json` 1.2.1 to 1.4.2: with `@unison/base` 7.20.0 and `parseNats t = catch do Decoder.run (Decoder.array Decoder.nat) t` added, `upgrade unison_json_1_2_1 unison_json_1_4_2` replies `I upgraded ...` and `list lib` then shows only the new json. Verified. (The diff between them is mostly the library's own nested base moving from 2.10.0 to 4.0.0, so user code over the JSON API survives.) I did not find a real pair of Share releases that breaks ordinary user code; `@unison/base` 7.19.0 to 7.20.0 adds about 13 terms and 2 types and would need scouting for a broken one, so idea 4 uses the offline recipe.

**What a library brings.** `lib.install @unison/routes/releases/7.0.2` is 181k terms and nests its own `lib` with `unison_base_7_8_3`, `unison_cloud_24_1_0`, `unison_codec_2_1_1`, `unison_http_8_0_0`, `unison_json_1_3_5` and `unison_uri_parser_2_1_5`. `find.all` sees one level of that.

**Judges and conflicts.** A judge should never compare UCM's reply text exactly. Check state: `branches` (no `merge-*`, `update-*`, `upgrade-*` temporary branch left, the current branch is the expected one), `todo` (nothing pending), `list lib`, `view`, `test`, `history`.

---

## The ideas you asked for

### 1. `workflow-merge-clean`: merge a branch with no conflicts

**Situation.** Two teammates worked from `main`: branch `alice` added `Temperature.toCelsius`, branch `bob` added `Temperature.toKelvin`, and a test for each. Alice wants both on `alice`.

**Setup.** Offline. Base codebase, then `branch alice`, `branch bob`, a `unison` block and `add`/`update` on each (see tricks). Start on `alice`. Do not merge in setup. Optionally have both branches also edit one shared doc term on different definitions so the merge has real work.

**Prompt.**
> I'm on the `alice` branch of the current project. Bring in the work on `bob` so I have both people's changes here, and make sure everything still passes.

**Judge checks** (state only).
- `view` shows `Temperature.toCelsius`, `Temperature.toKelvin` and their tests in `alice`; `test` passes everything.
- `history` shows a merge commit (two parents) rather than copies of the definitions retyped; the definitions have the same hashes as in `bob` (`diff.namespace` between `bob` and `alice` shows only alice's own additions).
- `bob` is unchanged (`diff.namespace` against its original hash, or the same definitions).
- No leftover temporary branch; current branch is `alice`.

**Traps.** Retyping `bob`'s code instead of merging (final state looks right, history gives it away). Merging the wrong way (`alice` into `bob`). Merging when the current branch is `main`. Asking the user to choose when nothing needs choosing. Running `merge bob` without the `/`.

**Learn topics.** `ucm/merge`, `ucm/branches`.

**Automation.** Easy. **Flaky:** the judge must tell a merge commit from a retyped copy; a worker that does a legitimate `update` of the same code would give a different history but the same hashes, so score on hashes plus no temporary branch and treat history as a soft check.

### 2. `workflow-merge-conflict`: merge a branch that conflicts

**Situation.** `alice` and `bob` both changed `Order.total`: alice made it add a shipping fee, bob made it apply a discount. A function `Order.summary` depends on `total`. The prompt says both changes are wanted.

**Setup.** Offline. `main` has `Order`, `total`, `summary` and a test (`total` of a fixed order is `100`). `alice` changes `total` to add shipping (`+ 5`) and updates the test; `bob` changes it to subtract a discount and updates the test. Start on `alice`, uncommitted merge not attempted. Run with `UCM_MERGETOOL=`.

**Prompt.**
> On the `alice` branch I need `bob`'s changes merged in. We want both the shipping fee from my branch and the discount from his, applied in `Order.total`, and the tests updated to match.

**Judge checks.**
- `view Order.total` contains both the fee and the discount (run `Order.total` on a fixed order: expected value computed by the judge from the task, for example `100 + 5 - 10 = 95`).
- Test `Order.total.tests` passes and has the new literal expected value; `test` passes everything.
- `Order.summary` still exists and typechecks (`todo` is empty).
- `history` shows a merge with `bob` as a parent; `branches` shows no `merge-bob-into-alice`; the current branch is `alice`.
- The merged `total` has no `<<<<<<<` text anywhere (`view` it; `find` for the markers is not possible in names, so rely on `view`).

**Traps.** Leaving conflict markers in the file and loading it anyway. Picking one side. Editing `alice` directly instead of merging (history). Forgetting that a second `update` is needed after `load`. The tests also conflict (both updated the same test) so the file has two conflicts.

**Learn topics.** `ucm/merge`, `ucm/temporary-branch`, `ucm/scratch-file`.

**Automation.** Medium. The judge has to decide that both behaviours are in the result without fixing the exact code, so it evaluates the function on an input. **Flaky:** a worker could write `total` in several correct ways; that is why the judge runs it. UCM's file hash in names and the mergetool env var (see tricks).

### 3. `workflow-lib-install-upgrade`: install a library from Share, then upgrade it

**Situation.** A project already depends on an old release of `@unison/json` (1.2.1) in one function, `parseNats`. The developer wants the newest JSON library.

**Setup.** Needs network (about 30 seconds for base plus two json installs; build once in `setup.md` like the base). Install base 7.20.0 (already there), `lib.install @unison/json/releases/1.2.1`, add `parseNats` and a test; pin everything. Do not install the new one: the worker finds the latest release itself.

**Prompt.**
> This project uses the JSON library for `parseNats`. Update it to the latest version of the library and make sure `parseNats` and its test still work.

**Judge checks.**
- `list lib`: exactly one json folder, `unison_json_1_4_2` (the latest release at the time the benchmark is built; the judge reads the latest release from the Share MCP tool or the task pins it in the judge as "at least 1.4.2"), the old `unison_json_1_2_1` is gone, `unison_base_7_20_0` still there, no other libraries added.
- `view parseNats` refers to the new library: `dependencies parseNats` lists names under `lib.unison_json_1_4_2` and none under the old folder.
- `parseNats` test passes and `parseNats "[1,2,3]"` is `Right [1, 2, 3]`.
- No temporary branch left; current branch `main`.

**Traps.** `lib.install` of the new version and no `upgrade`, leaving two copies (and code still on the old). Deleting `lib.unison_json_1_2_1` by hand, so `parseNats` loses its dependency and becomes a conflict-free but broken term. Installing at the wrong name (`@unison/json` main branch). Upgrading base instead.

**Learn topics.** `ucm/libraries`, `ucm/upgrade`, `ucm/share`.

**Automation.** Medium: needs the network at setup time, and "latest" moves. Pin the judge to the exact folder name for a known release and tell the worker "to 1.4.2" if you want it fully stable, or let it say "latest" and accept anything at or above 1.4.2. **Flaky:** Share availability, new releases, or a release that breaks `parseNats`. The offline variant of this idea is #4's recipe with a non-breaking v2.

### 4. `workflow-lib-upgrade-conflict`: an upgrade that breaks the project's own code

**Situation.** The project calls `lib.mylib_1_0_0.greet : Text -> Text` and pattern matches on `Opt.Opt n`. Version 2.0.0 of the library made `greet` take two arguments and gave `Opt` a second field. The developer wants version 2.0.0.

**Setup.** Offline, deterministic (the fork recipe in the tricks). The library is a local project `mylib` forked into `lib.mylib_1_0_0` and `lib.mylib_2_0_0`; project code `useGreet`, `useCfg`, `welcome : Text -> Text` (calls `greet`) and a test for `welcome`. The new version also has a doc saying how to migrate, in a doc term next to `greet` (so a worker who reads the library's docs gets the intended default for the second argument, for example `""`). A Share-based version is possible with `@unison/json` (needs a pair of releases that break user code, not found).

**Prompt.**
> The project depends on `mylib` 1.0.0 and I want it on 2.0.0 now. Do the upgrade and fix whatever breaks. Keep behaviour the same, the existing test has to keep passing.

**Judge checks.**
- `list lib`: only `mylib_2_0_0` (and base); `mylib_1_0_0` is gone.
- `dependencies welcome`, `useGreet`, `useCfg` all under `lib.mylib_2_0_0`.
- `test` passes; `welcome "bob"` gives the same result as in the original (the judge knows it from the setup).
- `todo` empty; `branches` shows no `upgrade-*` branch, current branch `main`.

**Traps.** Hand-editing every call and never running `upgrade` (the old folder remains, or gets deleted by hand). Running `upgrade` and giving up on the conflict file, leaving the temporary branch. Loading the file without `update` afterwards. `delete.namespace lib.mylib_1_0_0` first, which removes the code that needs to change. Changing the test to pass.

**Learn topics.** `ucm/upgrade`, `ucm/temporary-branch`, `ucm/scratch-file`, `ucm/libraries`.

**Automation.** Easy to medium. **Flaky:** the judge must accept different ways of choosing the second argument as long as behaviour is preserved (it compares output on fixed inputs), and different but valid ways of fixing `useCfg`.

### 5. `workflow-update-clean`: an update that touches dependents and finishes by itself

**Situation.** `Greeting.format` is used by two other functions. The developer wants `format` to add punctuation. Nothing's type changes, so nothing breaks, but the dependents' behaviour (and their tests) changes.

**Setup.** Offline. `main` has `Greeting.format`, `Greeting.welcome`, `Greeting.farewell` that call it, and tests for all three with literal expectations. Nothing to stage beyond that.

**Prompt.**
> `Greeting.format` should end every greeting with an exclamation mark. Change it, and update the tests that depend on it to match.

**Judge checks.**
- `Greeting.format "bob"` ends with `!` and `welcome`, `farewell` still call `format` (dependencies), not copies of its body.
- Tests pass; the expected values in tests contain the exclamation mark and are literals.
- `todo` empty; no temporary branches; the old test results are not left as stale (no definition with the old hash in the current branch: `diff.namespace` against the original shows `format` and the tests modified, nothing added under a different name like `format2`).
- `history` has an `update` step; no `delete` followed by `add` under the same name is needed to see it.

**Traps.** Adding `Greeting.format2` and changing callers by hand. `add` where `update` is needed (UCM replies that the name is taken or creates a conflict only when a name is duplicated). Editing the tests last and leaving `test` red. Not noticing that dependents' tests are separate terms.

**Learn topics.** `ucm/update`, `ucm/scratch-file`, `testing`.

**Automation.** Easy. **Flaky:** very little; the one soft spot is the "no copies" check, which is a `diff.namespace` read.

### 6. `workflow-update-type-conflict`: an update that changes a type and the typechecker rejects dependents

**Situation.** `Account` has fields `owner` and `balance`; `deposit`, `describe` and a test use it. The developer wants a new field, `currency : Text`, with default `"USD"` for the existing accounts.

**Setup.** Offline. `main` has `unique type Account = { owner : Text, balance : Nat }`, `Account.deposit`, `Account.describe`, an `Account.sample` value and tests. The worker issues the type change itself (a stanza with the typechecked block and then `update` in setup that fails would stage the temporary branch, but prefer the worker running it).

**Prompt.**
> Add a `currency : Text` field to `Account` and give every existing account `"USD"`. `deposit` and `describe` should keep working, and `describe` should now mention the currency, like `bob: 10 USD`. Make sure the tests pass.

**Judge checks.**
- `view Account` has the three fields; `Account.currency` exists (the record accessor).
- `Account.describe sample` ends with the currency; `deposit` keeps the currency of the input (run `Account.deposit` on an account with `"EUR"`).
- All tests pass; `todo` empty; `branches` shows no `update-*` temporary branch; current branch `main`.
- The old definitions that depended on `Account` were updated in place: the names are unchanged and there is no `Account2`.
- Dependents outside the file: add a second dependent that is not in the first file (`audit`) so the worker meets the multi-round case (see #11).

**Traps.** Creating a new type `Account2` to avoid the conflict. Defining `describe` with a new name. Running `update` repeatedly and not noticing it needs `load` first. Stopping on the temporary branch without the final `update`. Deleting the dependents. Constructor argument order (record constructor positional after the change). `Nat` vs `Int` if balance goes through a conversion.

**Learn topics.** `ucm/update` (the detail has this exact case), `ucm/temporary-branch`, `ucm/scratch-file`, `lang/records`.

**Automation.** Easy to medium. **Flaky:** the exact text format for `describe` (give the format in the prompt and let the judge check `endsWith`).

### 7. `workflow-http-json-server`: an HTTP server that decodes a JSON request and answers with JSON

**Situation.** The project has no libraries but base. The developer wants a small HTTP service: `POST /add` with the body `[1,2,3]` (JSON array of naturals) returns `{"sum": 6}`.

**Libraries that really exist on Share (checked with the Share tools, 2026-10-30).**

| Job | Project | What it is | Notes |
|---|---|---|---|
| HTTP server (preferred) | `@unison/routes` 7.0.2 | `Route` ability: `route`, `noCapture`, `s`, `ok.json`, `decodeJson`, `serveSimple`, `harness` for tests | public, `@unison` user. README has a full server example and a `harness` test. Builds on `@unison/http`. |
| HTTP types, client, low-level server | `@unison/http` 16.1.0 | "HTTP client and server and common types used for both" | public, `@unison`. A worker can serve with it directly, but `routes` is the friendly layer. |
| JSON (preferred) | `@unison/json` 1.4.2 | "JSON parsing, decoding, and encoding". `Decoder.run`, `Decoder.array`, `Decoder.nat`, `Json.object`, `Json.nat`; README is a tutorial with error reporting | public, `@unison`. |
| Archived | `@unison/httpserver` 4.1.1 | README starts with "Archived project! ... merged into `@unison/http`" | a trap: it shows up in `share-project-search http server` first. Search results label it private. |
| Other JSON | `@stew/json`, `@etorreborre/json-decoder`, `@etorreborre/json-encoder`, `@mitchellwrosen/json-decode`, `@mitchellwrosen/json-encode`, `@unison/json-core` | alternatives by individuals (`json-core` is marked private) | usable, but not under `@unison` and not what `routes` uses. |

What I verified by installing: `lib.install @unison/routes/releases/7.0.2` brings its own nested `lib` with `unison_json_1_3_5`, `unison_http_8_0_0`, `unison_cloud_24_1_0` and others, so `Json` and `Decoder` are already available through `find.all`. `serveSimple : '{IO, Route, Exception, Threads} () -> Nat ->{IO, Exception} '{IO, Exception} ()`. `decodeJson : '{g, Decoder} a ->{g, Route, Exception} a`, `ok.json : Json ->{Route} ()`. A first attempt at a handler failed with `needs the {Exception} ability, but this location only has access to the {Route} ability` because `decodeJson` raises, so the route must run under the handler `serveSimple` provides.

Dependency trap: installing `@unison/json` 1.4.2 next to `routes` gives a second `Json` whose type may not be the one `routes` expects (it uses 1.3.5). Using the names `routes` exposes through its nested library avoids that.

**Setup.** Base codebase only (the worker must discover and install). Needs the network for the worker's own installs, about 20 seconds. No setup transcript.

**Prompt.**
> I want a small HTTP service in this project. `POST /add` takes a JSON array of natural numbers in the request body and answers with JSON like `{"sum": 6}`. Anything that isn't valid JSON should get a 400. Put it in the codebase with a test, don't start the server for good.

**Judge checks.**
- `list lib` has `@unison/routes` (or `@unison/http`) at a real release and does not contain `unison_httpserver` (archived) or an individual's JSON library when `@unison/json` is available; JSON via `@unison/json`, either installed or the nested copy in `routes`.
- The handler and entry point exist, with types that mention `Route` (or `HttpRequest`), and `dependencies` reach `decodeJson` or `Decoder.run` and `ok.json` or `Json.object`: it does not parse JSON by hand with `Text.split` or build the answer with `++`.
- The test passes: a `harness` based test (`test.verify`, the form in the routes README) posts `[1,2,3]` and gets back a body whose JSON `sum` is `6`; the judge adds `[]` giving `0`, and a bad body giving 400. If the worker only tests the pure part (`sumJson : Text -> ...`) that is acceptable, but a route-level test is better (GREAT).
- Judge's own run: use `io.test` or a short transcript that starts the service on a free port, sends the request with `@unison/http` client and stops it (needs the IO transcript tool; not verified here).
- Rating: GREAT = `@unison/routes` + `@unison/json` with the decoder and a route test; PASS = works through `@unison/http` directly or hand-rolled JSON; FAIL = does not work or uses an archived/unrelated library.

**Traps.** Picking `@unison/httpserver` because it is first in the search and its README looks complete (it says archived). Never reading a README and guessing the API (`Route` names are not guessable: `s`, `noCapture`, `ok.json`). Mixing two `Json` types after installing json separately. Starting the server in the worker and hanging on `readLine`. Handling an invalid body with an exception that becomes a 500 rather than a 400 (the routes docs say `decodeJson` failures give 400 via `pickStatus`; the worker must check). `serveSimple` needs `Threads`. Using `Nat` for numbers that arrive as `Float` through `Decoder.float`.

**Learn topics.** `ucm/share` (search, README, `share.render`), `ucm/libraries`, `ucm/find`, `lang/abilities`, `testing`. Missing: a topic on choosing a library (prefer `@unison`, check the README for "Archived", check the release date) and on reading a library's docs through `docs` and `share.render`. An `ucm/io-run` topic for starting servers.

**Automation.** Medium to hard. The network is needed during the worker's run, not only the setup. **Flaky:** Share search ranking and version changes (judge by library name and a release, not by an exact version); a server test that binds a port; `harness` needs `Cloud`/`IO` abilities that may make `test` slow; the judge's HTTP call over localhost.

---

## More ideas (best first)

### 8. `workflow-merge-type-break`: a merge with no textual conflict that fails to typecheck

**Situation.** `bob` changed `Item.price` from `Nat` to `Float` and updated its callers on his branch; `alice` added a new function `Item.discounted` that uses `price` as a `Nat`. Neither touched the same definition, but together they do not typecheck.

**Setup.** Offline. `branch alice`, `branch bob`, as in the tricks. `merge /bob` in `scratch/alice` is a `:error` stanza (verified in the update case that "would no longer typecheck" gives the temporary branch; for merge, the same mechanism is described in `ucm/merge`).

**Prompt.** `> Merge bob's branch into mine (alice) and get everything working again.`

**Judge checks.** `Item.discounted` typechecks against the `Float` price and returns the right value on a fixed input; tests pass; `todo` empty; merge commit has `bob` as parent; no temporary branch; `bob` untouched.

**Traps.** `cancel` and redoing `bob`'s change by hand (history). Casting `price` back to `Nat`. Treating the lack of markers as a sign of a clean merge.

**Topics.** `ucm/merge`, `ucm/temporary-branch`. **Automation.** Medium. **Flaky:** merge may report this as a different kind of temporary-branch stop; check it by running once.

### 9. `workflow-merge-silent-combine`: both edited one definition on different lines

**Situation.** The guide says the merge is line by line, so two edits to different lines of one definition combine with no markers into something neither branch wrote. Setup: a function `report` of five lines; alice changes the title line, bob changes the footer line, and the combination is wrong because the footer refers to the title (bob changed a count that alice's new title duplicates).

**Prompt.** `> Merge bob into alice. Both of us changed report, and the result should read sensibly.`

**Judge checks.** `view report` shows both edits and the cross-reference is consistent; test expects the exact output string; merge commit exists.

**Traps.** Trusting an automatic merge that finished, without looking at the result. **Topics.** `ucm/merge` (its detail says to look at those definitions). **Automation.** Medium. **Flaky:** whether the combined result is "sensible" is subjective, so make the test fix a literal string.

### 10. `workflow-undo-update`: undo a mistaken update or merge

**Situation.** Someone ran an `update` that rewrote `parse` incorrectly and a later `update` built on it. The developer wants the branch back to how `parse` was before, without losing the later unrelated function `render`.

**Setup.** Offline, a few `update` stanzas in sequence on `main`.

**Prompt.** `> The last few changes to parse were wrong. Put parse back to how it was before them, but keep render.`

**Judge checks.** `parse` has the original hash (compare against the setup's saved hash through `diff.namespace` of the history entry); `render` exists; tests pass. **Traps.** `reset` to an older namespace hash, losing `render`; deleting and retyping. **Topics.** `ucm/history`, `ucm/codebase`; may need a missing topic on reverting one definition. **Automation.** Medium. **Flaky:** the judge needs the earlier hash (print it from the setup).

### 11. `workflow-update-multiround`: dependents that are not in the first file

**Situation.** Change a type; the first conflict file contains the type and the direct dependents. Fixing those exposes dependents of the dependents that are only reported on the next `update`. Setup: three layers (`Config` type, `load` uses it, `start` uses `load`, `report` uses `start`).

**Prompt.** `> Add a port : Nat field to Config, defaulting to 8080, and fix everything that depends on it.`

**Judge checks.** As in #6: all four definitions exist and typecheck, `todo` empty, no temporary branch, tests pass. **Traps.** Stopping after the first successful `load`; believing the update finished. **Topics.** `ucm/update` detail ("dependents that aren't in the file"). **Automation.** Easy. This can be folded into #6.

### 12. `workflow-json-encode-record`: encode and decode a record as JSON, no server

**Situation.** Smaller sibling of #7: a `Person` record to and from JSON text with the `@unison` JSON library.

**Setup.** Base codebase, a `Person` type, nothing installed.

**Prompt.** `> I need Person.toJson : Person -> Text and Person.fromJson : Text -> Either Failure Person (name and age), round tripping. Add them with tests.`

**Judge checks.** `list lib` has `@unison/json` (not `@stew/json` etc.); functions use `Json.object`/`Decoder.object`; round trip test; error case returns `Left` for bad JSON and a missing field; no hand-built strings with `++`.

**Traps.** Hand-rolling JSON text. Picking a library by an individual, or one marked private. `Decoder` with `at!` vs combinators. `Nat`-valued fields arriving as numbers. **Topics.** `ucm/share`, `ucm/libraries`, `lang/records`. **Automation.** Easy (network at the worker's run). **Flaky:** the library choice when several JSON libraries exist (use "GREAT/PASS/FAIL" for `@unison/json` / other / hand-rolled).

### 13. `workflow-lib-upgrade-two`: upgrade two libraries at once, one breaking

**Situation.** `lib.a_1_0_0`, `lib.b_1_0_0` (offline recipe); only `b` breaks code in 2.0.0. `upgrade a_1_0_0 a_2_0_0 b_1_0_0 b_2_0_0` takes both pairs. **Judge checks.** both old folders gone, both new present, project code uses the new ones, tests pass. **Traps.** Upgrading one at a time and leaving one old copy; leaving both versions. **Topics.** `ucm/upgrade`. **Automation.** Easy.

### 14. `workflow-merge-cancel`: a merge to abandon

**Situation.** `bob`'s branch is a bad idea (it deletes `render`). The developer tries merging it, sees it conflicts, and wants to give up and keep going on `alice` with only the part of `bob`'s work that is useful (a single function added in one definition). **Prompt.** `> I tried to merge bob into alice but his branch is a mess. Don't merge it, just bring over his new function Text.slug and its test.` **Judge checks.** `Text.slug` and test present and passing; `render` still exists; no merge commit with bob (history); no temporary branch. **Traps.** Merging and deleting later; `cancel` forgotten. **Topics.** `ucm/merge` (cancel), `ucm/branches`; missing: copying definitions between branches (`fork`, `merge` of a namespace, `view` and retyping). **Automation.** Medium.

### 15. `workflow-feature-branch`: the whole branch loop

**Situation.** A plain request to do the work on a branch and bring it back: create `feature`, add a function with a test, merge back into `main` and leave `main` as the current branch, with `feature` deleted when done. Setup is the base codebase. **Judge checks.** `branches`: `main` only; `main` has the function and a passing test; history has a merge or the update. **Traps.** Editing `main` directly; forgetting to switch back; leaving the temporary branch. **Topics.** `ucm/branches`, `ucm/merge`, `ucm/temporary-branch`. **Automation.** Easy. A good smoke test that the worker knows the verbs.

### 16. `workflow-pull-share-branch`: pull a branch from Share (hard)

**Situation.** Merge a published branch of a small public project into a local one with `pull` (needs a stable Share branch with a known conflict; none picked). **Automation.** Hard: depends on the network and on someone else's branch not changing. Skip unless a branch we control is published.
