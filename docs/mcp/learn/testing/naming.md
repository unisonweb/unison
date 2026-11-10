Tests for a function or type `foo` are named `foo.tests.<name>`, with conventional names for the common kinds. The tests of `foo` then sit together in the namespace `foo.tests`.

- `foo.tests.examples` holds tests with hardcoded inputs and expected outputs.
- `foo.tests.props` holds property-based tests (see [property-based tests](learn:testing/props)). If there are several with different setup, call them `foo.tests.<description>Props`, with a `<description>` that says what is different.
- Tests that need the same setup or generated inputs belong in one test, with `labeled` naming each check (see [the test functions](learn:testing/verify)), so the setup isn't repeated. Don't add `labeled` to a test that does one thing.
- A test for a type `Foo` is `Foo.tests.<name>`, the same way.

## Detail

**Example.**

````
``` unison
test> double.tests.examples = test.verify do
  labeled "zero" do ensureEqual (double 0) 0
  labeled "three" do ensureEqual (double 3) 6

test> double.tests.props = test.verify do
  Each.repeat 100
  n = Random.natIn 0 1000
  labeled "doubling is adding to itself" do
    ensureEqual (double n) (n + n)
  labeled "result is even" do
    ensure (Nat.isEven (double n))
```
````

**Running a group.** `test double.tests` runs every test saved under `double.tests` (see [running tests](learn:testing/running)).
