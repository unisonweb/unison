# `specialize`: interpreted, specialized and by hand

Branch `specialize` at `0efffb1e3` plus the higher-order rows of
[`specialize-benchmarks.md`](../../../../../../unison-src/transcripts-manual/specialize-benchmarks.md),
optimized build (`--work-dir .stack-work-opt`), Apple M1, the `jit` testing codebase. One
run of `printTime` per row; the specialization happens before the timing. See
[Specialize.md](../../Specialize.md).

| Row | JIT off | JIT on |
| --- | --- | --- |
| fib 20: interpreted | 16.3 ms | 744 µs |
| fib 20: specialized | 1.71 ms | 30.9 µs |
| fib 20: by hand | 1.72 ms | 36.8 µs |
| polynomial, 100000 points: interpreted | 134 ms | 6.54 ms |
| polynomial, 100000 points: specialized | 15.4 ms | 553 µs |
| polynomial, 100000 points: by hand | 15.0 ms | 484 µs |
| map with a lambda, 10000 elements: generic | 1.42 ms | 255 µs |
| map with a lambda, 10000 elements: specialized | 1.19 ms | 232 µs |
| foldLeft with a lambda, 10000 elements: generic | 1.08 ms | 168 µs |
| foldLeft with a lambda, 10000 elements: specialized | 684 µs | 154 µs |
| foldLeft with a lambda, 10000 elements: by hand | 419 µs | 155 µs |
| filter and map as a fold, 10000 elements: generic | 1.91 ms | 251 µs |
| filter and map as a fold, 10000 elements: specialized | 1.14 ms | 153 µs |
| filter and map as a fold, 10000 elements: by hand | 944 µs | 160 µs |
| five functions from a list, 100000 points: generic | 88.5 ms | 11.3 ms |
| five functions from a list, 100000 points: specialized | 15.6 ms | 611 µs |
| five functions from a list, 100000 points: by hand | 15.2 ms | 485 µs |
| fib 20, interpreter in CPS: interpreted | 27.9 ms | 3.50 ms |
| fib 20, interpreter in CPS: specialized | 2.52 ms | 274 µs |
| state handler around a loop, 10000 rounds: generic | 2.66 ms | 441 µs |
| state handler around a loop, 10000 rounds: specialized | 474 µs | 3.25 µs |
| state handler around a loop, 10000 rounds: by hand | 396 µs | 3.20 µs |
| fib 20, interpreter with an ability to bail: interpreted | 19.3 ms | 898 µs |
| fib 20, interpreter with an ability to bail: specialized | 3.47 ms | 96.9 µs |

The last five rows are from `049ef9b8f`, after handlers were fused (state passing, with a
call in tail position a tail call); the rows above them didn't change.

Findings:

- Specializing an interpreter on a program gives about 10× under the interpreter and 12–24×
  under the JIT, and lands within 15% of the program written by hand (fib under the JIT is
  within noise of it).
- A function argument known to a loop is inlined into it: with the JIT on, `foldLeft` and the
  filter-and-map fold match the hand-written loops, and a known list of five functions
  applied in turn becomes straight-line code, 18× faster than the generic version.
- `map` gains least (10–15%): building the list dominates.
- With the JIT off, the specialized `foldLeft` is 1.6× the hand-written loop although the JIT
  makes them equal: the output carries interpretive overhead the JIT removes (the peeled
  first round, literal lets). Not looked into yet.
- A state handler around a loop, fused, matches the hand-written loop (the JIT reduces
  both to a few microseconds); before the tail-call shortcut the state-passing loop
  returned a tuple from every round and was 2.7× slower than the generic handler under
  the JIT. The interpreter with an ability to bail gains 5.6× without the JIT and 9.3× with
  it, the abort becoming a returned value.
- The interpreter in continuation-passing style keeps its continuations as closures after
  specializing (the whistle generalizes them), so it gains less than the direct-style one
  relative to its starting point, but still about 10×.

## With join points

At the commit that adds join points ([Specialize.md](../../Specialize.md), "Abilities"),
the same suite with them on and off (`UNISON_SPECIALIZE_JOIN=off`), the specialized rows:

| Row | JIT off, on | JIT off, off | JIT on, on | JIT on, off |
| --- | --- | --- | --- | --- |
| fib 20 | 1.75 ms | 1.75 ms | 30.9 µs | 31.2 µs |
| polynomial, 100000 points | 15.6 ms | 15.6 ms | 553 µs | 553 µs |
| map with a lambda, 10000 elements | 1.19 ms | 1.20 ms | 241 µs | 242 µs |
| foldLeft with a lambda, 10000 elements | 692 µs | 694 µs | 159 µs | 159 µs |
| filter and map as a fold, 10000 elements | 1.15 ms | 1.15 ms | 158 µs | 158 µs |
| five functions from a list, 100000 points | 15.7 ms | 15.7 ms | 612 µs | 612 µs |
| state handler around a loop, 10000 rounds | 468 µs | 468 µs | 3.25 µs | 3.25 µs |
| fib 20, interpreter with an ability to bail | 3.43 ms | 3.43 ms | 93.4 µs | 93.3 µs |
| fib 20, interpreter in CPS | 2.52 ms | 2.52 ms | 273 µs | 275 µs |

No row changes. Two earlier versions of the clean-up pass did cost the interpreter with an
ability to bail: with every join point left a call, 4.53 ms and 113 µs; with only the small
ones copied, 4.27 ms and 107 µs. Its loop has two join points, each with one call once the
loop's function is left alone, and each call of one was a call per round.

## With calls unfolded under unknown matches, and direct calls within a group

At the commits that unfold a call under an unknown match unless the whistle blows
([Specialize.md](../../Specialize.md), "Recursion and termination") and that emit a call
of a function of the same group with all its arguments as a direct call
([`emitFunction`](../../MCode.hs)).

| Row | JIT off | JIT on |
| --- | --- | --- |
| fib 20: specialized | 1.76 ms | 36.8 µs |
| fib 20: by hand | 1.76 ms | 36.8 µs |
| polynomial, 100000 points: specialized | 15.6 ms | 554 µs |
| polynomial, 100000 points: by hand | 15.2 ms | 485 µs |
| map with a lambda, 10000 elements: generic | 1.44 ms | 250 µs |
| map with a lambda, 10000 elements: specialized | 697 µs | 239 µs |
| foldLeft with a lambda, 10000 elements: specialized | 425 µs | 159 µs |
| foldLeft with a lambda, 10000 elements: by hand | 424 µs | 160 µs |
| filter and map as a fold, 10000 elements: specialized | 950 µs | 164 µs |
| filter and map as a fold, 10000 elements: by hand | 951 µs | 165 µs |
| five functions from a list, 100000 points: specialized | 15.7 ms | 613 µs |
| five functions from a list, 100000 points: by hand | 15.3 ms | 485 µs |
| state handler around a loop, 10000 rounds: specialized | 397 µs | 3.25 µs |
| state handler around a loop, 10000 rounds: by hand | 397 µs | 3.21 µs |
| fib 20, interpreter with an ability to bail: specialized | 3.24 ms | 112 µs |
| fib 20, interpreter in CPS: specialized | 2.22 ms | 170 µs |

Findings:

- With the JIT off, specialized code now equals the code written by hand in every row that
  has one, where `foldLeft` was 1.6 times it and the state loop 1.2 times. Two things did
  that: a function argument is inlined into the loop it is passed to (the call of it
  under the loop's test is unfolded), and a call between residual functions is direct.
- A call of a function of the same group went through the general application, which
  checks the number of arguments as it runs; every residual function is in one group, so
  every call between them did. With only the unfolding change, `fib` was 1.94 ms and
  43.9 µs for the same optimized code as the one by hand.
- `map` with the JIT off goes from 1.20 ms to 697 µs, and the interpreter in
  continuation-passing style from 275 µs to 170 µs with it on.
- `fib` under the JIT was 30.9 µs, less than by hand, and the interpreter with an ability
  to bail 93 µs: the earlier output did two levels of the recursion in each call, by an
  accident of which residual functions were inlined. Both are now the loop as written.

## Handlers whose case goes on after resuming, or resumes twice

At the commit that makes a loop under such a handler a function returning the `handle`'s
value ([Specialize.md](../../Specialize.md), "Abilities"). The other rows are as in the
table above.

| Row | JIT off | JIT on |
| --- | --- | --- |
| tick counter around a recursion, 1000 deep: generic | 65.2 ms | 211 ms |
| tick counter around a recursion, 1000 deep: specialized | 180 µs | 29.2 µs |
| tick counter around a recursion, 1000 deep: by hand | 63.5 µs | 6.86 µs |
| coin flips in a loop, 10 flips: generic | 1.12 ms | 1.36 ms |
| coin flips in a loop, 10 flips: specialized | 280 µs | 52.3 µs |
| coin flips in a loop, 10 flips: by hand | 166 µs | 25.6 µs |

Findings:

- The tick counter's handler resumes and then adds one, 1000 deep, so the generic version
  captures and resumes a continuation inside a continuation each round: 65 ms, and more
  with the JIT on. Specialized it is a recursion that passes a closure, 363 times faster
  with the JIT off and 7,000 times with it on.
- It is still 2.8 times the recursion by hand (4.3 times under the JIT): it makes a closure
  each round for the code after the call, where the one by hand adds to a tuple.
- The coin flips are 4 times faster than the generic version with the JIT off and 26 times
  with it on, and 1.7 to 2 times the one by hand.
- One run had the interpreter in continuation-passing style at 216 µs under the JIT, and
  the next at 167 µs, as before: that row varies between runs.


## With loops under several handlers returning a `handle`'s value

At the commit that extends the value-returning form to several handlers, makes a
continuation a function only when a residual function resumes it, and splits `List.at` on
a known list ([Specialize.md](../../Specialize.md), "Abilities"). No row of this file is
under two fused handlers, so this run checks that nothing moved:

| Row | JIT off | JIT on |
| --- | --- | --- |
| fib 20: specialized | 1.77 ms | 37.0 µs |
| fib 20: by hand | 1.78 ms | 36.9 µs |
| map with a lambda, 10000 elements: specialized | 702 µs | 252 µs |
| foldLeft with a lambda, 10000 elements: specialized | 430 µs | 164 µs |
| foldLeft with a lambda, 10000 elements: by hand | 429 µs | 162 µs |
| filter and map as a fold, 10000 elements: specialized | 976 µs | 167 µs |
| state handler around a loop, 10000 rounds: specialized | 408 µs | 3.24 µs |
| state handler around a loop, 10000 rounds: by hand | 404 µs | 3.23 µs |
| bail interpreter: specialized | 3.26 ms | 115 µs |
| tick counter around a recursion, 1000 deep: specialized | 183 µs | 29.7 µs |
| tick counter around a recursion, 1000 deep: by hand | 63.9 µs | 6.89 µs |
| coin flips in a loop, 10 flips: specialized | 281 µs | 52.8 µs |
| coin flips in a loop, 10 flips: by hand | 169 µs | 25.9 µs |
| fib 20, interpreter in CPS: specialized | 2.22 ms | 248 µs |

Every row is within 3% of the last run except the interpreter in continuation-passing
style under the JIT, which has varied between 167 µs and 248 µs over three runs with the
same generated code.


## With the bridge around calls of unknown functions

At the commit that runs a call of an unknown function under a bridge
([Specialize.md](../../Specialize.md), "Abilities"). No row of this file calls an unknown
function under a fused handler, so this run checks that nothing moved:

| Row | JIT off | JIT on |
| --- | --- | --- |
| fib 20: specialized | 1.77 ms | 36.8 µs |
| fib 20: by hand | 1.77 ms | 36.8 µs |
| polynomial, 100000 points: specialized | 15.7 ms | 554 µs |
| map with a lambda, 10000 elements: specialized | 698 µs | 242 µs |
| foldLeft with a lambda, 10000 elements: specialized | 424 µs | 161 µs |
| filter and map as a fold, 10000 elements: specialized | 952 µs | 165 µs |
| five functions from a list, 100000 points: specialized | 15.7 ms | 613 µs |
| state handler around a loop, 10000 rounds: specialized | 397 µs | 3.45 µs |
| state handler around a loop, 10000 rounds: by hand | 397 µs | 3.21 µs |
| bail interpreter: specialized | 3.25 ms | 113 µs |
| tick counter around a recursion, 1000 deep: specialized | 179 µs | 29.2 µs |
| coin flips in a loop, 10 flips: specialized | 278 µs | 52.2 µs |
| fib 20, interpreter in CPS: specialized | 2.20 ms | 250 µs |

With the JIT off every specialized row is within 1% of the last run. With it on they are
within 4%, except the state handler's loop (3.45 µs against 3.26 µs, a row of a few
microseconds) and the interpreter in continuation-passing style, which varies between
runs as before.
