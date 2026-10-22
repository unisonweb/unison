# Floats, and `Universal.murmurHashUntyped`

Two `jitSuite` rows added on 2026-10-04, optimized build, interpreter against `UNISON_JIT=on`.

| Benchmark | Interpreter | `jit=on` | Speedup |
| --- | --- | --- | --- |
| Float: sqrt, sin, cos, arithmetic and conversions, 100000 times | 33.7 ms | 1.01 ms | 34× |
| murmurHashUntyped of Some (i, "x"), 10000 times | 6.86 ms | 264 µs | 26× |

The float loop ran with one trampoline entry and no exits, so nothing falls back. The broad
suite's `List.map murmurHash` row (363 µs before, 362 µs after) is unchanged: it uses the
typed `Universal.murmurHash`, which serializes the value and stays a call-out.
