Handling things that can fail or be missing: `Optional` and `Either` for values, and abilities for exceptions.

- `Optional a` is `Some a` or `None`, for a value that may be absent. `Optional.getOrElse default opt`, `Optional.map f opt`.
- `Either e a` is `Left e` (a failure, by convention) or `Right a`. `Either.mapRight f`, `Either.mapLeft f`, and `Either.fold onLeft onRight`.
- Functions that can fail can instead use an [ability](learn:lang/abilities): `Exception` (the error is a `Failure`), `Throw e` (the error is of type `e`), or `Abort` (no error details). Each is turned back into a value by a handler: `catch do ...` gives `Either Failure a`, `toEither do ...` gives `Either e a`, and `toOptional! do ...` gives `Optional a`.
- Raise an `Exception` with `Exception.raise (Generic.failure "message" value)`, a `Throw` with `throw error`, and an `Abort` with `Abort.abort`.

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
