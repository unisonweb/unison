Running things at the same time, with the `Threads` ability in the base library: structured concurrency, where every thread has finished or been cancelled before the call returns. Don't start threads by hand.

- The functions are in `IO.Threads`, and you can call them as `Threads.parMap`, `Threads.race` and so on. They need the `IO`, `Exception` and `Threads` [abilities](learn:lang/abilities).
- `Threads.parMap f list` runs `f` on every element of `list` at the same time and returns the results in order. If one fails, the others are cancelled.
- `Threads.race a b` runs two computations (delayed, see [delayed computations](learn:lang/delayed)) and gives `Either a b` for whichever finishes first, cancelling the other. `Threads.multiRace` does the same for any number: `Threads.multiRace (submit -> List.foreach (task -> submit task) tasks)` starts one racer per element of `tasks` and returns the first result.
- `Threads.both a b` runs two at the same time and returns both results. `Threads.timeout duration do ...` fails if the computation takes longer.
- `Threads.sleep duration` waits. A duration is `Duration.milliseconds +200` or `Duration.seconds +1`, and these take an `Int`, so the literal needs a `+` (see [operators](learn:lang/operators)).
- `Threads.run do ...` handles `Threads` and leaves `IO` and `Exception`. Put it where the `Threads` computation ends: inside your function if callers shouldn't need to know, or leave `Threads` in its type so the caller decides. A function that uses `IO` has `IO` in its type.
- IO code can't be run by a `> watch` line or by `test>`. Run it with `run` (see [running code](learn:ucm/run)) and test it with `io.test` (see [IO tests](learn:testing/io)), both of which need `transcript-io`.
- To test that things really ran at the same time, time the call with `Clock.monotonic()` and check the elapsed time is near that of one task, not the sum of all of them.
- To time something in a test, `start = Clock.monotonic()`, then `elapsed = Clock.monotonic() Duration.- start` is a `Duration`: `ensure (elapsed Duration.< Duration.seconds +1)`. There is no `Duration.toMilliseconds`.

## Detail

**An example,** with its test and how to run it:

````
``` unison
slowly : Text ->{IO, Exception, Threads} Text
slowly t =
  Threads.sleep (Duration.milliseconds +100)
  Text.toUppercase t

shout : Text -> Text ->{IO, Exception} (Text, Text)
shout a b = Threads.run do Threads.both (do slowly a) (do slowly b)

shout.tests.concurrent : '{IO, Exception} [Result]
shout.tests.concurrent = do test.verify do
  start = Clock.monotonic()
  result = shout "hello" "world"
  elapsed = Clock.monotonic() - start
  ensureEqual result ("HELLO", "WORLD")
  ensure (elapsed < Duration.milliseconds +180)

small : '{IO, Exception} (Text, Text)
small = do shout "a" "b"
```

``` ucm
> add
> io.test shout.tests.concurrent
> run small
```
````

`run small` prints `("A", "B")`, and the two sleeps of a tenth of a second finish in about a tenth of a second, not two: `both` runs them at the same time, and `parMap` does the same for a whole list.

**Measuring time.** `Duration.asMilliseconds d` is a `Float`.

**Racing.** `Threads.run do Threads.race (do Threads.sleep (Duration.milliseconds +300); "slow") (do "quick")` is `Right "quick"`.

**Failing and cancelling.** If any computation fails, the others are interrupted. An empty list given to `multiRace` never finishes, so check for it first and raise an `Exception` yourself (see [errors](learn:lang/errors)).

**Documenting.** The function's documentation should say which abilities it needs and how to run it (see [documentation](learn:lang/documentation) for functions that use IO).

**Docs.** `docs Threads.parMap`, `docs Threads.multiRace` and `docs Threads.run` have more.
