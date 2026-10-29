Defining functions, calling them, and writing small ones inline.

- A function is a signature and a definition, with its parameters after the name: `addNums : Nat -> Nat -> Nat` then `addNums n1 n2 = n1 + n2` (with `use Nat +` first, see [operators](learn:lang/operators)).
- Call it with space-separated arguments: `addNums 4 5`. Use parentheses to group: `addNums (double 2) 5`.
- Functions are curried: `addNums 1` is a function that adds one.
- A lambda is `x -> x + 1` or `acc a -> acc + a`, written in parentheses as an argument: `List.map (x -> x + 1) [1, 2, 3]`. `cases` is a lambda that [matches patterns](learn:lang/cases): `List.map (cases (a, b) -> a + b) pairs`. `_ -> 10` ignores its argument.
- A block is the indented lines under `=`. Lines `x = expr` bind a name for the lines after it, the last line is the value, and `_ = expr` discards a value.
- A local function in a block can have its own signature, or none.
- `use Nat +` or `use List :+` brings names into scope for the rest of the block.

## Detail

**The `let` form.** `f x = let` followed by an indented block is the same as the block directly under `f x =`.

**Recursion.** A function can call itself, and doesn't need a signature to do so.

**Higher-order functions** take functions as arguments, as in `List.foldLeft (acc a -> acc + a) 0 [1, 2, 3]`. Their types show it (see [signatures](learn:lang/signatures)).
