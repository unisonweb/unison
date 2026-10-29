"I couldn't figure out what + refers to here", or "I couldn't resolve any of these names" with several suggestions, means a name matches more than one definition.

For an operator, the message is:

````
I couldn't figure out what + refers to here:

   12 |     h +: t -> go (acc + h) t

The name + is ambiguous. Its type should be: acc -> 𝕩 -> acc

I found some terms in scope that have matching names and types. Was any of these what you wanted?

(Float.+) : Float -> Float -> Float
(Int.+) : Int -> Int -> Int
(Nat.+) : Nat -> Nat -> Nat
````

The operator `+` belongs to `Nat`, `Int` and `Float`, and Unison couldn't tell which from the code, because the type of `acc` isn't known. Write `use Nat +` at the start of the block, or `Nat.+` where you use it, or add a type signature (see [operators](learn:lang/operators)).

For a constructor in a [pattern](learn:lang/cases), the message lists the matches in the Suggestions column:

````
I couldn't resolve any of these names:

Name     Type          Suggestions
Single   constructor   Bytes.base32Hex.Hex32Piece.Single
                       Two.Single
````

Your type's constructor has the same name as a constructor in a library, so the pattern is ambiguous. Choose a name that is specific to your type, or write the constructor with its type: `Two.Single`.

## Detail

An ambiguous name in an expression, not a pattern, usually clears up when the types are known, because Unison picks the one that fits. It is in untyped local functions and in patterns that the ambiguity shows up.
