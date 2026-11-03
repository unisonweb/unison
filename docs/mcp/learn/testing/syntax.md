The syntax that test code uses: `test>` lines, `do` blocks, and bindings inside a block that call random generators or enumerate cases.

- `test> name = expression` in a `unison` block defines `name` and runs it right away. It must have type `[Result]`.
- `do` starts a [block that isn't run yet](learn:lang/delayed): `test.verify do <lines>` hands the lines to `test.verify`, which runs them, and decides what to do when a check fails or a generator produces values. Indent the lines of the block under it.
- Inside such a block, `n = Random.natIn 0 1000` binds `n` to a random number, and `m = Each.range 0 10` runs the rest of the block once for each value of `m`. These are calls to [abilities](learn:testing/abilities), which `test.verify` provides.
- A check such as `ensureEqual a b` is a line of the block. If it fails, the test stops there with a failure.
- `use Nat +` at the start of a block lets you write `+` for `Nat.+` in it, and `use Each range` lets you write `range` for `Each.range` (see [operators](learn:lang/operators) and [functions](learn:lang/functions)).

## Detail

**Two ways to use `do`.** `labeled "name" do ...` and `Random.listOf (do Random.natIn 0 100) do Random.natIn 0 20` pass blocks that aren't run yet to a function, which runs them when it needs to. `Random.listOf` runs the first block once per element, and the second to pick how many.

**An IO test** starts with `do` before `test.verify`, which makes the whole test a block that isn't run until `io.test` runs it (see [IO tests](learn:testing/io)).

**Functions in a test.** `cases n -> body` is a function written inline, as in `test.prop 100 cases n -> ...` (see [`cases`](learn:lang/cases)).

**Numbers.** `1` is a `Nat` (non-negative), and `+1` is an `Int`. Literals like `0.5` are `Float`s (see [terms](learn:lang/terms)). Use `Nat.+` or `use Nat +` when `+` is ambiguous.
