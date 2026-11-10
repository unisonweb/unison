An ability is Unison's way of describing what a function can do besides returning a value. The test code uses four abilities, and `test.verify` provides all of them.

- `Exception` lets a check fail. `ensure` and `ensureEqual` use it.
- `Label` records labels. `labeled` and `label` use it.
- `Random` generates random values: `Random.natIn`, `Random.listOf` and so on.
- `Each` produces several values, running the rest of the block once for each: `Each.repeat 100`, `Each.range 0 10`.
- A function's type shows its abilities: `ensure : Boolean ->{Exception} ()` may fail, and `Random.natIn : Nat -> Nat ->{Random} Nat` uses `Random`. See [abilities](learn:lang/abilities) for what they are in general.

## Detail

**Why `n = Random.natIn 0 1000` works in a block.** `test.verify` is a handler: it runs the block, and answers each ability request. For `Random` it supplies a number. For `Each` it re-runs the rest of the block for each value. For `Exception` it ends the test with a failure.

**More.** `docs Random` and `docs Each` are long and list everything each ability offers.
