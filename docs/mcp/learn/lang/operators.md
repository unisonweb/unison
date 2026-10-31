Infix operators, defining your own, and the operators that compose functions.

- Operators are two-argument functions written between their arguments: `1 Nat.+ 2`. To put one first, use parentheses: `(Nat.*) 2 3`, not `Nat.* 2 3`, which gives a parse error that points at nothing. Some, like `++`, `&&` and `|>`, have no type prefix.
- Arithmetic operators are named for their type (`Nat.+`, `Int.*`), so `+` alone can be ambiguous. Write `use Nat +` once at the start of a block to write `+` in it, or write `Nat.+` each time.
- **In a helper with no signature, nothing says what the operands are,** so `n * n` is ambiguous even in a function on `Nat`s. Add a signature to the helper, or write `use Nat * +` as the first line of the `unison` block, which covers the whole block.
- **`==` also belongs to a type** (`Nat.==`, `Text.==`), and most types have none. `===` and `!==` compare any two values of the same type, by their structure, so use `===` on records, `Optional`s, lists and your own types. `Universal.lt`, `Universal.gt`, `Universal.lteq` and `Universal.gteq` order any two values. `Universal.==` doesn't exist.
- **There are no type classes.** `Ord a =>` doesn't parse, and `<` doesn't work on a type variable. A generic function uses `Universal.lt`, or takes the comparison as an argument.
- `x |> f` is `f x`, and `f <| x` is also `f x` (which avoids parentheses). `f >> g` runs `f` and then `g`, and `f << g` runs `g` and then `f`.
- Define one with `(^^) x y = ...` or `x ^^ y = ...`, which can be called as `2 ^^ 3`.
- Operators have the usual precedence (see the [detail](learn:lang/operators/detail)): `2 + 3 * 4` is `2 + (3 * 4)`, and `1 + 2 >= 3 + 4` is `(1 + 2) >= (3 + 4)`. When in doubt about how an expression groups, use parentheses.

## Detail

**Precedence.** From loosest to tightest. The same name has the same precedence whatever its type prefix, so `Nat.+` and `+` are alike.

| Level | Operators |
| --- | --- |
| 0 (loosest) | `\|\|`, `\|` |
| 1 | `&&`, `&` |
| 2 | `==`, `!=`, `===`, `!==` |
| 3 | `<`, `>`, `<=`, `>=` |
| 4 | `+`, `-` |
| 5 | `*`, `/`, `%` |
| 6 (tightest) | `^`, `^^`, `**` |

Operators in the same level group from the left. An operator that isn't in the table (`|>`, `<|`, `>>`, `<<`, one you define) groups strictly left to right with the operators around it, so `1 * 2 $ 3 * 4` is `((1 * 2) $ 3) * 4`. Parenthesize when mixing them with others.

Equality binds tighter than `&&` and `||`, so `true || false == true` is `true || (false == true)`, and `true || false && true` is `true || (false && true)`.

**Allowed characters.** An operator name is made of `!$%^&*-=+<>~\/|:`.

**An example.**

````
``` unison
(^^) : Nat -> Nat -> Nat
(^^) x y = Nat.pow x y

> 2 ^^ 3
> Some 2 |> Optional.filter Nat.isEven
```
````
