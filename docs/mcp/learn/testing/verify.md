`test.verify` runs a block of checks and returns a `[Result]`. `ensure`, `ensureEqual` and the rest are the checks, and `labeled` and `label` make a failure say what was being checked.

- `test.verify : '{g, Exception, Each, Random, Label} () ->{g} [Result]` runs a block, which can use [abilities](learn:testing/abilities) for failing, repeating, random numbers and labels. A test passes if no check in the block fails.
- `ensure : Boolean ->{Exception} ()` fails the test if its argument is `false`.
- `ensureEqual a b` fails, showing both values, if `a` and `b` aren't equal. Also `test.ensureNotEqual`, `test.ensureLess`, `test.ensureGreater`, `test.ensureLessOrEqual`, `test.ensureGreaterOrEqual`, which show both values, and `test.ensureEqualWith eq a b`, which compares with the function `eq`.
- `ensureWith label boolean` is `ensure` that prints `label` on failure. `ensuring do <boolean>` is `ensure` for a Boolean that is only computed when needed, and that keeps the computation in the failure.
- `labeled "name" do ...` wraps a block so that a failure inside it is reported under `name`. `label "x" value` records a value that is shown if the test fails.
- `test.check : Boolean -> [Result]` is a one-line test for a Boolean, without `test.verify`.

## Detail

**A test with labels.**

````
``` unison
test> double.tests.examples = test.verify do
  labeled "zero" do ensureEqual (double 0) 0
  labeled "three" do ensureEqual (double 3) 6
```
````

**Recording values.** `label "n" n` makes a failure show `n: 3`, so you can see the value that made the test fail. Use it on generated values (see [property-based tests](learn:testing/props)).

**Raising a failure yourself.** `test.raiseFailure "message" value` fails the test with a message.

**Reading the docs.** `docs test.verify` has a tutorial. `docs ensureEqual`, `docs labeled` and so on document the others (see [`find`](learn:ucm/find) and where they live in [libraries](learn:testing/libraries)).
