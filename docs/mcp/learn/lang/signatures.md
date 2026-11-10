How to read a type signature, such as `List.map : (a ->{e} b) -> [a] ->{e} [b]`.

- `->` separates the parameters from the result. `Nat -> Nat -> Nat` takes two `Nat`s and returns a `Nat`.
- A lowercase name is a type variable: `a -> a` works for any type `a`.
- Parentheses make a parameter that is itself a function: `(Nat -> Nat) -> Nat -> Nat` takes a function, then a `Nat`.
- `[a]` is a list, `Optional a` a value that may be missing, `(a, b)` a tuple, and `{ name: Text, age: Nat }` a [record](learn:lang/records).
- Braces after an arrow list the [abilities](learn:lang/abilities) the function may use: `Nat ->{IO, Exception} ()`. `{}` or none means it is pure. A lowercase name in the braces, as in `->{e}` or `->{g}`, stands for whatever abilities the function argument uses.
- A quote, as in `'Text` or `'{IO} Text`, is a [delayed computation](learn:lang/delayed): a value that hasn't been computed yet.
