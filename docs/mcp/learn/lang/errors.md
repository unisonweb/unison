Handling things that can fail or be missing: `Optional` and `Either` for values, and abilities for exceptions.

- `Optional a` is `Some a` or `None`, for a value that may be absent. `Optional.getOrElse default opt`, `Optional.map f opt`.
- `Either e a` is `Left e` (a failure, by convention) or `Right a`. `Either.mapRight f`, `Either.mapLeft f`, and `Either.fold onLeft onRight`.
- Functions that can fail can instead use an [ability](learn:lang/abilities): `Exception` (the error is a `Failure`), `Throw e` (the error is of type `e`), or `Abort` (no error details). Each is turned back into a value by a handler: `catch do ...` gives `Either Failure a`, `toEither do ...` gives `Either e a`, and `toOptional! do ...` gives `Optional a`.
- Raise an `Exception` with `Exception.raise (Generic.failure "message" value)`, a `Throw` with `throw error`, and an `Abort` with `Abort.abort`.
- **Which to use.** For a function that fails in its user-facing type, `Exception` is the usual choice, and `Throw e` when the failure should carry a value you choose, such as the input that was bad. `Abort` is for control flow inside a function and rarely appears in a signature, since it can't say what went wrong. `Exception.raiseGeneric "message" value` is the short way to raise.
- **Turn an `Optional` into a failure** with `Optional.orThrow error opt` (in a `Throw`) and, to process a whole list, `List.map` over a function that does that, inside one handler. `Throw.toEither do List.map (k -> Optional.orThrow k (Map.get k table)) keys` is `Left` the first key that isn't in the table, or `Right` all the values, in order. The first failure stops everything after it, so there is no loop to write.
- **When a function has a failing version and an `Optional` or `Either` version, write one and get the other from it** with a handler. If `Config.parse : Text ->{Throw Text} Config` is the main one, `Config.parseOrLeft t = Throw.toEither do Config.parse t`, and an `Optional` version is `Either.toOptional` of that or `Abort.toOptional!` of an `Abort` version. Don't write the logic twice, and make the plain failing one the main one.

## Detail

**An example of each.**

````
``` unison
divBy : Nat -> Nat ->{Abort} Nat
divBy a b = match b with
  0 -> Abort.abort
  n -> a / n

divThrow : Nat -> Nat ->{Throw Text} Nat
divThrow a b = match b with
  0 -> throw "Cannot divide by zero"
  n -> a / n

divException : Nat -> Nat ->{Exception} Nat
divException a b = match b with
  0 -> Exception.raise (Generic.failure "Cannot divide by zero" b)
  n -> a / n

> toOptional! do divBy 1 0
> toEither do divThrow 1 0
> catch do divException 1 0
```
````

**In a test,** use the `ensure` functions, which use `Exception` (see [testing](learn:testing/verify)).
