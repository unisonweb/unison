The Unison language: the minimum you need to read and write Unison code, including how to document it. Code is written in `.u` files, or in `unison` blocks of a [transcript](learn:ucm/transcript). When you write a definition, [document it](learn:lang/documentation).

- A definition is a name, an optional type signature on the line above, and a value: `double : Nat -> Nat` then `double n = n * 2`. Blocks are made with indentation, and the last line of a block is its value (see [terms](learn:lang/terms) and [functions](learn:lang/functions)).
- Functions are called by writing the arguments after the name, separated by spaces: `double 4`.
- Use recursion or functions like `List.map` to express loops and iteration (see [control flow](learn:lang/control)), and `Optional` instead of `null` (see [errors](learn:lang/errors)).
- Text is `Text`, taken apart with `Pattern` (see [text](learn:lang/text)). Mutable state is a `Ref` in a `Scope` (see [mutable state](learn:lang/mutable)), and things run at the same time with `IO.Threads` (see [concurrency](learn:lang/concurrency)).
- If code that looks right fails to parse, see [layout](learn:lang/layout).
- Effects such as IO, failure or randomness show up in a function's type, as [abilities](learn:lang/abilities): `Nat ->{IO, Exception} ()`.
- You pattern match on values with [`cases` and `match`](learn:lang/cases), and define your own types with [`type`](learn:lang/data) or [`ability`](learn:lang/abilities).
