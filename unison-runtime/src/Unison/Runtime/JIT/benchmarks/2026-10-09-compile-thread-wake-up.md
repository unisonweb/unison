# Waking the compile thread (2026-10-09)

How long a request waits before the compile thread picks it up, from a timestamp in the
request, logged under `UNISON_JIT_LOG`. Optimized build, `--jit on`. "wake" is the change:
the compile thread forked with `forkOn` onto a capability other than the forking thread's,
and `jitRequestGroup` yielding after it queues a request; "without" is the old behaviour
(`forkIO`, no yield), which the build used for this comparison had behind a switch that was
removed afterwards.

First request of a small program (the batching benchmark's three definitions), three runs
each:

| | run 1 | run 2 | run 3 |
| --- | --- | --- | --- |
| without | 306 µs | 1,232 µs | 35 µs |
| with | 36 µs | 35 µs | 39 µs |

Every request of `jitSuite`, one run each; the requests that waited behind a compile in
progress (over 5 ms) are shown separately:

| | requests | idle median | idle p90 | idle max | behind a compile | overall max |
| --- | --- | --- | --- | --- | --- | --- |
| without | 73 | 37 µs | 58 µs | 4.6 ms | 27 | 988 ms |
| with | 75 | 36 µs | 46 µs | 3.4 ms | 32 | 978 ms |

Takeaways. When the compile thread is idle a request is served in about 40 µs, which is the
STM wake-up plus the cache reads, and that is the same on the fast build. The slow case is a
compile thread sharing the interpreter's capability: it then runs at that capability's next
scheduling point, a GC or the context-switch tick, and that is where the 1.2 ms came from, a
thousand or more interpreted calls on an optimized build. `forkIO` from the thread that
starts the runtime put it there whenever the interpreter ran on the same capability, which
happened in the small program and mostly didn't in the suite. The change makes the fast case
the only case; it changes nothing else (same modules, same timings, the batching benchmark's
row at 118 µs both ways). The lag that remains, and the one that matters for first-install
latency, is a compile already in progress: a request arriving during one waits 10 ms to a
second, which is the argument for bounding a module's size.
