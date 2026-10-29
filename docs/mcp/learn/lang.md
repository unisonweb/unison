The Unison language: the minimum you need to read and write Unison code. Code is written in `.u` files, or in `unison` blocks of a [transcript](learn:ucm/transcript).

- A definition is a name, an optional type signature on the line above, and a value: `double : Nat -> Nat` then `double n = n * 2`. Blocks are made with indentation, and the last line of a block is its value (see [terms](learn:lang/terms) and [functions](learn:lang/functions)).
- Functions are called by writing the arguments after the name, separated by spaces: `double 4`.
- Use recursion or functions like `List.map` to express loops and iteration (see [control flow](learn:lang/control)), and `Optional` instead of `null` (see [errors](learn:lang/errors)).
- Effects such as IO, failure or randomness show up in a function's type, as [abilities](learn:lang/abilities): `Nat ->{IO, Exception} ()`.
- You pattern match on values with [`cases` and `match`](learn:lang/cases), and define your own types with [`type`](learn:lang/data) or [`ability`](learn:lang/abilities).
