In the current Unison project, I need `timedPair : Nat -> Nat ->{IO, Exception} (Nat, Nat)`, which waits `a` milliseconds and `b` milliseconds at the same time, not one after the other, and returns `(a, b)` once both waits are over. So `timedPair 300 200` returns `(300, 200)` after about 300 milliseconds, not 500. Add it to the codebase, with tests that show the waits overlap.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
