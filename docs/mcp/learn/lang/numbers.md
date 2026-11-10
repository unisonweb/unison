Which number type to use: `Nat`, unless the values can really be negative.

- **Default to `Nat`** (a whole number, zero or more) for counts, sizes, indexes, ages, prices in cents, quantities, limits and ids. Write the type as `Nat`, not `Int`, and write literals without a sign (`5`). Use `Int` (`+5`, `-3`) only when the domain has negative values, such as a temperature, a balance that can go below zero, or a difference you need the sign of. Use `Float` for fractional numbers (`3.14`), and `Natural` and `Integer` (see below) only when numbers must not overflow.
- **`Nat.-` doesn't go below zero:** `10 Nat.- 1000` is `0`, not an error and not a negative number. When the answer could be negative and you need to know, subtract as `Int` with `Nat.subtractToInt 10 1000` (`-990`) or compare first.
- **Convert on purpose:** `Nat.toInt` (always works), `Int.truncate0` (negative becomes `0`) or `Int.toNat` (an `Optional`, `None` for a negative), `Nat.toFloat`, `Float.toNat` and `Float.toInt` (`Optional`s), and `Nat.toText` and `Nat.fromText` (an `Optional`). Floors and rounding are on `Float`: `Float.floor`, `Float.ceiling`, `Float.round`.
- **Division of `Nat`s truncates:** `7 Nat./ 2` is `3`, and `Nat.mod 7 3` is `1`. Dividing by zero is an error: check first (see [errors](learn:lang/errors)).
- **Operators are named for the type** (`Nat.+`, `Int.*`), so write `use Nat +` once at the start of a block (see [operators](learn:lang/operators)). A literal's type decides which: `1` is a `Nat`, `+1` is an `Int`, `1.0` is a `Float`.

## Detail

**An example.** A discount that can't make a total negative is natural with `Nat`:

````
Order.total : Nat -> Nat
Order.total subtotal =
  use Nat -
  subtotal - 10
````

`Order.total 5` is `0`. If a total below zero should be a different answer, check `subtotal < 10` first, or use `Nat.subtractToInt`.

**Overflow.** `Nat` and `Int` are 64 bits: `maxNat` is `18446744073709551615`, and adding to it wraps around. For numbers that can be bigger, base has `Natural` and `Integer` (arbitrary size) and `Decimal`; see `docs Natural`.

**Don't pick `Int` to be safe.** A signature with `Int` where `Nat` is meant makes callers write `+` on every literal and lets negative values in that the code doesn't handle.
