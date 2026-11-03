Taking values apart with `match` and `cases`, using patterns.

- `match value with` followed by indented cases, one per line, `pattern -> result`. They are tried from the top, and must cover every possibility.
- `cases` is a function that matches its arguments: `f = cases` followed by the cases. With several arguments, separate the patterns with commas: `n1, n2 | n1 === n2 -> "same"`.
- Patterns: a literal (`"Pie"`, `0`), a variable (`n`, which matches anything and names it), `_` (matches anything), a constructor (`Some n`, `Left e`, `Point x y`), a tuple (`(a, b)`), a list (`[]`, `[a]`, `[a, b] ++ rest`, `head +: tail`, `init :+ last`), a [record](learn:lang/records) (`{ name: n }`), and `name@pattern`, which also names the whole value.
- A guard, `| condition`, after a pattern limits when the case applies: `n | n < 3 -> "small"`.
- Every constructor field needs a pattern: `_` if you don't care.
- Put specific cases before general ones. A later case that an earlier one already covers is reported.

## Detail

**A function with `cases`.**

````
``` unison
describe : [Nat] -> Text
describe = cases
  [] -> "empty"
  [a] -> "one"
  first +: middle :+ last -> "many"

pick : Optional Nat -> Optional Nat
pick = cases
  s@(Some n) | n > 3 -> s
  _ -> None
```
````

**`match`.** `match f with "Pie" -> "slice"` and then `_ -> "???"`, indented under the `match` line, is the same as the `cases` form applied to `f`.

**List patterns.** `a +: b +: rest` doesn't work, because `b` must be a list. Write `[a, b] ++ rest`.

**Abilities.** A [handler](learn:lang/abilities) matches requests with patterns in braces, `{ KVStore.get key -> resume }`.
