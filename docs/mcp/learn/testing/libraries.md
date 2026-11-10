The test functions, `Each` and `Random` are in the base library, `@unison/base`, on [Unison Share](learn:ucm/share). Install it once per project with `lib.install @unison/base/releases/latest` (see [libraries](learn:ucm/libraries)), and then the names work without a prefix.

- The functions: `test.verify`, `ensure`, `ensureEqual`, `ensureWith`, `ensuring`, `labeled`, `label`, `test.raiseFailure`, `test.arbitrary.nats`, `test.arbitrary.ints`, `test.prop`.
- The abilities: `Each`, `Random`, `Exception`, `Label` (see [abilities](learn:testing/abilities)).
- Read a function's documentation with `docs <name>`, for example `docs test.verify`, and its source with `view <name>` (see [`find`](learn:ucm/find)).

## Detail

**Documentation worth reading.** `docs test.verify` is a tutorial on the whole testing library. `docs Each` and `docs Random` list what each ability offers.

**Searching.** [`find`](learn:ucm/find) `ensure` lists the checks, and `find.all test.` includes the library.

**The language's own page.** The Unison documentation has a guide to testing at https://www.unison-lang.org/docs/usage-topics/testing/.
