Property-based tests run the same checks on many generated inputs, using the `Each` and `Random` abilities inside `test.verify`.

- Put `Each.repeat 100` at the start of the block to run the rest of the block 100 times, each with different random values.
- `Random.natIn lo hi` is a random `Nat` from `lo` up to but not including `hi` (it fails if `hi` isn't greater than `lo`). `Random.intIn`, `Random.nat`, `Random.boolean` and `Random.oneOf [..]` make other random values.
- `Random.listOf gen n` makes a list: `gen` generates each element, and `n` generates its length, both as blocks: `Random.listOf (do Random.natIn 0 1000) do Random.natIn 0 20`.
- `test.arbitrary.nats n` produces `n` numbers, starting with edge cases (`0`, `1`, `maxNat`, `2`) and then random ones. `test.arbitrary.ints n` does the same for `Int`s. Use them to find overflow bugs.
- `Each.range lo hi` runs the rest of the block for every number from `lo` up to but not including `hi`, to test exhaustively.
- `test.prop 100 cases n -> ...` is a shortcut for `test.verify` that runs its body 100 times and passes it the run number `n`, counting from 1. Use `n` to make the inputs grow as the run goes on: small cases first, larger ones later.

````
``` unison
test> List.tests.reverseProps = test.verify do
  Each.repeat 100
  xs = Random.listOf (do Random.natIn 0 1000) do Random.natIn 0 20
  label "xs" xs
  labeled "reversing twice is the identity" do
    ensureEqual (List.reverse (List.reverse xs)) xs
  labeled "length is preserved" do
    ensureEqual (List.size (List.reverse xs)) (List.size xs)
```
````

## Detail

**Edge cases first.** `test.arbitrary.nats` reveals assumptions. `x = test.arbitrary.nats 10`, `y = test.arbitrary.nats 10`, then `ensureGreaterOrEqual (x + y) x` fails on `y = maxNat`, since the sum overflows to `0`.

**A fixed seed.** Random values change on each run. `test.verifyWithSeed 7 do ...` uses the seed `7`, so the values are the same every time, which helps when reproducing a failure.

**`test.prop`.** `test.prop 100 cases n -> body` takes the number of runs and a function of the run number, so you don't need `Each.repeat`. The run number is useful for scaling the size of the generated data, so the early runs are small and easy to read and the later ones are big:

````
``` unison
test> List.tests.reverseGrowing = test.prop 100 cases n ->
  use Random nat
  l = listOf nat (natUpTo n)
  ensureEqual (List.reverse (List.reverse l)) l
```
````

Here `natUpTo n` picks the length of the list, from `0` up to `n`, so the lists get longer as `n` goes from 1 to 100. `test.prop 1 ...` runs the body once. Use `docs test.prop` for more.

**More generators.** `docs Random` lists them all, including text, characters, shuffling and floats, and `docs Each` lists the ways to enumerate cases (see [abilities](learn:testing/abilities)).
