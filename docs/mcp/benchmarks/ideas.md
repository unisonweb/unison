# Benchmark ideas for basic coding, syntax and the base library

A draft for approval. Nothing here is built yet. After approval I'd write each task's `prompt.md` and judges, then take a baseline of the whole set before touching any topic.

**What's in the set already** (baselined once): `list-xf` (alias of `List.map`), `list-sift` (`List.filter`), `list-xf-keep` (`List.filterMap`), `list-pick` (a small new function, documented and tested). All four implementations passed. The reuse judges gave FAIL, FAIL, PASS (a wrapper, now partial credit) and n/a.

**How I picked these.** I listed what `@unison/base` has under `data.List` (about 700 terms), `data.Map` (about 530), `Set`, `Bag`, `Stream`, `Text`, `Nat`, `Optional` and `abilities`, and wrote tasks whose good solution is a short composition of those, plus a few that need real recursion, a data type, a record, or an ability. The traps are the ones agents actually hit so far: `Nat` vs `Int`, constructor names that clash with base, ambiguous operators, `find` skipping `lib`.

## The tasks

Each row's prompt describes behaviour and gives one example, and never names the base function. "Reuse angle" says what a strong solution reuses, and what the reuse judge looks for.

### List processing

| # | Folder | The ask | Reuse angle | Traps / edge cases |
|---|---|---|---|---|
| 1 | `list-run-lengths` | `List.runLengths : [a] -> [(a, Nat)]`. `[a,a,a,b,c,c,a,a]` gives `[(a,3),(b,1),(c,2),(a,2)]`. | `List.groupConsecutive` then `List.map`. | Equal values that are not adjacent must not merge. Empty list. Needs `Eq a`. |
| 2 | `list-neighbors` | `List.neighbors : [a] -> [(a, a)]`. `[1,2,3,4]` gives `[(1,2),(2,3),(3,4)]`. | An alias of `List.slidingPairs`. | One element and empty list give `[]`. |
| 3 | `list-most-common` | `List.mostCommon : [a] -> Optional a`. | An alias of `List.mostFrequent`. | Empty gives `None`. The judge checks ties against what `mostFrequent` does. |
| 4 | `list-running-totals` | `[1,2,3,4]` gives `[1,3,6,10]`. | `List.scanLeft` plus dropping the seed. | Empty list. Does it include the starting zero? |
| 5 | `list-rotate-left` | `List.rotateLeft : Nat -> [a] -> [a]`. `rotateLeft 2 [1,2,3,4,5]` is `[3,4,5,1,2]`. | `take`, `drop` and `++`, with a modulo. | `n` larger than the length. Empty list (division by zero if it divides by the size). |
| 6 | `list-merge-sorted` | `List.mergeSorted : [a] -> [a] -> [a]` for two ascending lists, in one linear pass. | None to reuse. New tail-recursive code over two lists. | `sort (a ++ b)` is correct but not linear, so the judge fails it. Equal heads. One list empty. |

### Working with `Map`

| # | Folder | The ask | Reuse angle | Traps / edge cases |
|---|---|---|---|---|
| 7 | `map-word-counts` | `wordCounts : Text -> Map Text Nat`, ignoring case, splitting on whitespace. | `Text.words`, `Text.toLowercase`, then `Map.fromListWith` or `Bag.counts`. | Repeated whitespace. Empty text. Uppercase and lowercase merge. |
| 8 | `map-invert` | `Map.invert : Map k v -> Map v [k]`. `{1:"a", 2:"b", 3:"a"}` gives `{"a":[1,3], "b":[2]}`. | `Map.toList` then `Map.fromListWith`. | Keys within a group stay ascending. `fromListWith` combines in an order that can reverse them. |
| 9 | `map-add-counts` | `Map.addCounts : Map k Nat -> Map k Nat -> Map k Nat`, summing shared keys. | An alias or wrapper of `Map.unionWith (+)`. | `+` is ambiguous without `use Nat +`. Keys in only one map are kept. |
| 10 | `map-top-keys` | `Map.topKeys : Nat -> Map k Nat -> [k]`, the `n` keys with the biggest values, ties by smaller key first. | `Map.toList`, `List.sortBy`, `List.take`. | `n` larger than the map. Ties. Descending sort. |
| 11 | `map-group-by-length` | `Map.groupByLength : [Text] -> Map Nat [Text]`, keeping input order inside each group. | `Map.fromListWith`, `List.groupMap` or similar. | Combining with `(++)` reverses the order. This is the one a careless solution gets wrong. |

### Project Euler style (two)

Base has `Integer` and `Natural` (arbitrary size, `Natural.pow`, `Integer.lcm`, `Integer.toText`), so big-number problems are fair game. The prompts say performance matters (the judge's input is big enough that a naive version times out). The prompt gives one small case; the judge adds the real one.

| # | Folder | The ask | Reuse angle | Judge inputs (not in the prompt) |
|---|---|---|---|---|
| 12 | `euler-factorial-digits` | Sum of the digits of `n!` as a `Natural`. Prompt shows `10 -> 27`. | `Natural` arithmetic, `Stream` or `List.foldLeft` over a range, `Natural.toText` and `Text.toCharList`, or `Natural.divMod`-style loop. | `100 -> 648`. `0 -> 1`. Using `Nat` overflows silently from `21!`. |
| 14 | `euler-nth-prime` | The `n`th prime. Prompt shows `6th -> 13`. Reasonably fast. | `Stream` plus `Stream.filter`, or a bounded trial division over earlier primes. | `100 -> 541`. `1 -> 2`. |

Dropped from the first draft: multiples of 3 or 5, even Fibonacci, largest prime factor, palindrome product, Pythagorean triplet, Collatz, digit powers. I can bring any back.

### Types, records, data

| # | Folder | The ask | What it exercises | Traps / edge cases |
|---|---|---|---|---|
| 21 | `types-shape-area` | A `Shape` type with circle, rectangle and triangle, `Shape.area`, and `Shape.largest : [Shape] -> Optional Shape`. | `lang/data`, `lang/cases`, `Float`, `List.maximumOn`. | Constructor names that clash with base. Empty list gives `None`. Float equality in tests. |
| 22 | `types-person-record` | A `Person` record (`name`, `age`), `Person.birthday`, `Person.oldest`. | `lang/records` (field modifiers), `List.maximumOn`. | Generated `age.modify` or `age.set` rather than rebuilding the record. |
| 23 | `tree-insert-fold` | A binary search tree `Tree a` with `Tree.insert`, `Tree.toList` (in order) and `Tree.depth`. | Recursion over a user type, `Ord`. | Duplicates. The recursion judge doesn't demand tail calls here. |

### Text patterns

Base has `Pattern` (`Text.patterns.*`, `Pattern.captures`, `Text.splitOnPattern`, `Text.capturePatternsWith`, `Pattern.isMatch`). The judge looks for use of `Pattern` rather than hand-rolled character loops, but a correct hand-rolled solution is `PASS`, not `FAIL`, since these are less obvious than `List.map`.

| # | Folder | The ask | Reuse angle | Traps / edge cases |
|---|---|---|---|---|
| 24 | `text-extract-numbers` | `Text.numbers : Text -> [Int]`, every integer in the text, in order. `"a1 b-22, c333"` gives `[1, -22, 333]`. | `Text.patterns.int` with `Pattern.captures`, or `Text.capturePatternsWith`. | Negative sign. Adjacent digits and letters (`x12y`). No numbers gives `[]`. |
| 25 | `text-parse-log-line` | `LogLine.parse : Text -> Optional LogLine` for `"[ERROR] 12:30 disk full"` into a record (level, hour, minute, message). | A `Pattern` with captures, `Nat.fromText`. | Wrong shape gives `None`. Message containing `]` or `:`. Two-digit padding. |
| 26 | `text-split-words` | `Text.splitCamel : Text -> [Text]`. `"parseHttpRequestV2"` gives `["parse","Http","Request","V2"]`. | `Text.splitOnPattern` with a lookahead, or `Pattern` captures. | Runs of capitals (`"HTMLParser"`). Empty text. All lowercase. |
| 27 | `text-caesar` | `Text.caesar : Int -> Text -> Text`, shifting letters and wrapping, other characters unchanged. | `Char` and `Nat`/`Int` conversion. | Negative shifts. A shift larger than 26. Upper and lower case. The `Nat`/`Int` subtraction trap. |

### Abilities and effects

These are the part of Unison an agent is least likely to have seen elsewhere, so there are more of them. All of them are new code with a handler; the judges check the behaviour through `run` and the structure by viewing.

| # | Folder | The ask | What it exercises | Traps / edge cases |
|---|---|---|---|---|
| 28 | `abilities-coro` | Define `ability Coro a b where yield : a -> b`. Then `Coro.feed : '{Stream a, g} r -> ('{Coro a b, g} r2) ->{g} ...`, composing a producer `Stream` with a consumer `Coro` into a `Stream`. The prompt gives the exact signature and an example: a `Stream` of `1,2,3` into a `Coro` that yields each value doubled gives the stream `2,4,6`. | Writing two handlers that interlock: one handles `Stream` by resuming the `Coro` continuation, the other handles `Coro` by pulling from the stream. `Stream.uncons`, `Stream.pipe` as models. | The handler must be written over `Request`, recurse after each step, and decide what to do when the stream ends and when the `Coro` finishes first. Two flavours: resume value vs. just output. |
| 29 | `abilities-random-seeded` | A function `shuffleSeeded : Nat -> [a] ->{} [a]` that shuffles reproducibly: the same seed gives the same result, and the result is a permutation. | `Random`, `Random.shuffle`, `Random.splitmix` or `Random.lcg` as handlers. | Must not use `IO`/`Random.run` (not reproducible). Empty and one-element lists. The prompt doesn't name the handler. |
| 30 | `abilities-random-dice` | `rollDice : Nat -> '{Random} [Nat]` rolling `n` six-sided dice, and `histogram : Nat -> Map Nat Nat` for a seeded run of `n` rolls, with the count for each face. | `Random.natIn` or `Random.oneOf`, `Random.listOf`, a handler, `Bag.counts` or `Map.fromListWith`. | Faces are 1 to 6 inclusive (`natIn` bounds). Counts sum to `n`. Purity: pure given a seed. |
| 31 | `abilities-store-label` | `Tree.label : Tree a -> Tree (Nat, a)` numbering the leaves left to right, using the `Store` ability. | `Store.get`, `Store.put`, `Store.withInitialValue`, `Store.modify`. | Order of traversal. A solution that threads the counter by hand is correct but misses the point, so the reuse judge rates it `PASS`. |
| 32 | `abilities-collect-log` | An ability `Log` with `log : Text -> ()` and a handler `Log.collect` returning the result and the messages in order. | Handler recursion over `Request`. | Order of messages. A handler that forgets to re-handle the continuation. |
| 33 | `abilities-each-search` | Using `Each` (or `Abort`), find all pairs `(a, b)` of numbers from 1 to `n` with `a < b` and `a*b` a perfect square. | `Each.toList`, `Each.each`, `Abort`. | Order of results. Using `Each` as a loop with a side effect. |

### Error handling

(Your message cut off at "an example of er". I read it as error handling; tell me if you meant something else.)

| # | Folder | The ask | What it exercises | Traps / edge cases |
|---|---|---|---|---|
| 34 | `errors-parse-all` | `parseAll : [Text] -> Either Text [Nat]`, the first text that fails, or all the numbers. | `Nat.fromText`, `Either`, `Optional`, `lang/errors`. | `Nat.fromText` returns an `Optional`. Stops at the first failure. Empty list is `Right []`. |
| 35 | `errors-safe-div` | A function `average : [Int] ->{Exception} Int` that raises on an empty list, plus a wrapper `averageOrNone : [Int] -> Optional Int` using `Abort.toOptional` or `Exception.catch`. | `Exception`, `Exception.catch`, `Abort`, `Abort.toOptional`, `Either`, and the names `raise`/`Exception.raise`. | Division by zero in `Int`. The right ability for a failure that carries no information (`Abort`) vs one that does (`Exception`). |
| 36 | `errors-retry` | `retry : Nat -> '{g, Exception} a ->{g} Either Failure a` runs a delayed computation up to `n` times and returns the first success, or the last failure. | `Exception.catch`, `Either`, delayed computations (`'`). | `n = 0`. Not evaluating the computation eagerly (it must be a delayed value, so effects happen once per try). |

### Other base knowledge

| # | Folder | The ask | What it exercises | Traps / edge cases |
|---|---|---|---|---|
| 37 | `set-common-elements` | `Set.commonToAll : [Set a] -> Set a`. | `Set.intersect` folded; `Set.unions` as the contrast. | The empty list of sets (should be the empty set, not an error). |
| 38 | `text-palindrome` | `Text.isPalindrome`, ignoring case, spaces and punctuation. | `Text`, `Char` predicates, `Text.reverse`. | `"A man, a plan, a canal: Panama"`. Empty text. |

That is 38 ideas, 34 of them new since the baseline. My proposed first batch is 24; it keeps your three Euler tasks, adds text patterns and abilities, and keeps one alias, one composition and one new-code task in each family:

- Lists: #1, 2, 4, 6
- `Map`: #7, 8, 9, 11
- Euler: #12, 13, 14
- Types: #21, 22
- Text patterns: #24, 25
- Abilities: #28 (`Coro`), 29 (`Random`), 31 (`Store`), 32 (`Log`)
- Errors: #34, 35


## Judges for every task

- **`behavior`** (always). Evaluates the function on the prompt's example, then on inputs the prompt doesn't show (edge cases, and the Euler "real" case). It checks the type signature. Ends `PASS` or `FAIL`.
- **`tests`** (always). Tests exist under `<name>.tests`, they pass, they have literal expected values, and they cover the empty case and one edge case from the task. Ends `PASS` or `FAIL`.
- **`reuse`** (when the task has an existing function behind it). Three ratings, as now: `GREAT` is the exact alias or a composition of base functions, `PASS` is a wrapper that could be eta reduced or a mostly-base solution with a hand-rolled piece, and `FAIL` is a reimplementation of something base has.
- **`efficiency`** (Euler tasks, `list-merge-sorted`). Runs the judge's big input and fails if it doesn't finish quickly, or if the code is not tail recursive where a loop is needed.
- **`handler`** (ability tasks). Views the handler and checks it handles `Request` correctly: recurses after resuming the continuation, handles the end of the stream and of the `Coro`, and doesn't use `IO` or `unsafe` shortcuts. Ends `PASS` or `FAIL`.
- **`docs`** (a few tasks only, since it is the most expensive judge): #1, #8, #21, #28. Same shape as `list-pick/docs-quality`.

## Prompt conventions

- Written as a need ("I need `List.neighbors : …`"), never "implement", and never naming the base function.
- Always: "Add it to the codebase, with tests."
- Euler prompts say it should be fast, and give a size that rules out a naive loop.
- Reuse tasks keep "don't look at the source of existing `List` functions (docs are fine)". Other tasks allow reading source, which is what a real user's agent does.

## Decisions

- First batch approved (24), plus two structured-concurrency tasks (`threads-slow-squares`, `threads-first-done`) that need the IO transcript tool. Built in the folders next to this file.
- `abilities-coro` gives the semantics and an example but not the type. The `ability-polymorphic` judge checks that both thunk arguments and the result share an ability variable (compare `Stream.map`), and proves it by running with an extra ability.
- `Random` handlers are not named in the prompt; the agent has to find them.
- Convention: functions that return a `Stream` (or any ability-using computation as a value) take and return thunks. Otherwise just execute the effects: put the ability requirement on the final arrow and let the caller wrap the call in `do` to delay it.
- Text-pattern and error-handling tasks are in.

## Topics to write (not started; waiting for the baseline)

- **Ability polymorphism.** Writing higher-order functions and functions that take thunks: the `g` variable, `'{g, Stream a} r -> '{g, Stream b} r` versus a closed `'{Stream a}`, and why a closed signature rejects callers that use other abilities. Use `Stream.map` as the model.
- **Mutable state with `Scope`.** `Scope.run` with `Scope.ref`, `Ref.read`, `Ref.write` and `Scope.ref.modify`: the efficient way to keep a counter or accumulator across a recursion, preferred over `Store`. Also arrays in `Scope`.
- **Documenting a function that uses IO.** Recommended shape: an example term `foo.docs.examples.name`, shown with `@source{...}`; then the `run foo.docs.examples.name` command in a `raw` block; then, when the program returns a value and prints nothing, the result in a `@typecheck` block. (Use a `raw` block for the output only when the program prints to the console.)
- **Layout (the offside rule).** A common source of errors. From `unison-syntax/src/Unison/Syntax/Lexer/Unison.hs` and some transcript experiments:
  - **What opens a block:** `=`, `->` (only the arm of a `match ... with` or `cases`), `match`, `handle`, `with`, `if`, `then`, `else`, `cases`, `where`, `let`, `do`, `type`, `ability` (and `structural`/`unique`), and the brackets `(`, `{`, `[`.
  - **The column:** the block's column is the column of the first token after the opener, wherever it is, even on the same line. A later line at that column starts a new item (a virtual semicolon, except inside `(`, `{`, `[` and `match`), a line to the left closes the block, and a line to the right continues the current item.
  - **The usual mistake:** indent a continuation relative to the start of the line, not to the first token after the `=` or `->`. `a = 1 +` with `2` on the next line fails at any indent up to the column of the `1`. `1 -> "one"` with `++ "!"` on the next line fails the same way. `p = Pattern.join` with the list on the next line at four spaces fails, because `Pattern` starts at column five and the list is at column five (equal means a new statement). To avoid it, put the body on its own line (`xs =` then an indented body), or indent continuations further than the first token.
  - **Brackets and `match`, `if`, `then`, `handle`:** dedenting never closes them (only the matching `)`, `}`, `]`, or `else`/`with` does), so a multi-line list, call or record in brackets can use any indent: `xs = [ 1` with `, 2` and `]` at a smaller indent parses, as do `g = (` with `)` at the left margin and a `type P = {` record with `}` at column one.
  - **An empty block** (`foo =` followed by a line at the same or smaller indent) is an error: "I expected a block after this".
  - **Working multi-line shapes:** `if c then a` with `else b` aligned under the `if`; `if`, `then` and `else` each on its own line; `match x with` with arms at one indent and an arm's body on the next line, indented; `cases` arms aligned; a call with one argument per line, each indented more than the function.
- **Effects as values vs executed.** When to take and return thunks (building streams, delayed computations) and when to run the effect and put the ability on the last arrow.

## Open questions

1. Should the reuse rating for composition tasks (`Map.invert`, `groupByLength`) stay `GREAT`/`PASS`/`FAIL` as written in the judges?
2. After the diverse baseline, which topics do we write first: ability polymorphism and effects as values, as above?

## UCM idea: put what needs attention at the top of the scratch file

(From Paul.) Whenever UCM writes a scratch file for an unfinished transaction (a `delete` with dependents, an `update` that needs fixing, a merge with conflicts), the things that need a person, such as conflicts and definitions that no longer typecheck, should come first, followed by a marker comment and then everything else, for example:

```
-- Note: All definitions in this file have been temporarily removed from <branchname>
-- Deleting a definition from this file will cause it to be deleted on `update`
-- ** Full dependents of change below **
```

Why: an agent can then be told to read and edit only down to the marker, and aim its edits at the definitions above it, without reading every transitive dependent, which saves tokens. What the files look like today, checked with the built binary: the file for a `delete` has two header comment lines and then the direct dependents in alphabetical order, with no ordering by urgency and no marker. This is a UCM change (where the file is written), not a `learn` change. Until it exists a `learn` topic can only say "read the whole file"; once it does, `ucm/update` and `ucm/rename-and-delete` should say "read up to the marker". Worth a benchmark of its own: the `workflow-update-type-conflict` and `workflow-delete-with-dependents` tasks, with the tokens the worker spends reading the file as a metric.
