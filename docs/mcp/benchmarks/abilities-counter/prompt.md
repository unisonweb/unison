In the current Unison project, I need an ability `Counter` with one operation `next : () -> Nat`, which gives the next number each time it is called, and a handler `Counter.from`, which runs a computation that uses `Counter`, starting the numbers at a given value: `Counter.from 10 do (next(), next(), next())` is `(10, 11, 12)`. Add it to the codebase, with tests and documentation.

Don't look at the source code of existing functions in `lib` or in base (`view`, `source` or similar on a definition outside this project). Reading documentation (`docs`) is fine.
