A delayed computation is a value that isn't computed until you force it, written with `do`. Tests, `run` and handlers use them.

- `do <expression>` delays the expression: `delayed = do Nat.increment 5`. `do` takes everything to its right, so put it in parentheses when more arguments follow: `foo (do bar 1) 2`.
- Force a delayed computation by applying it to `()`, as in `delayed()`. It is just a function call with `()` as the argument. `()` binds tighter than anything else, so `blah delayed() x` is `blah (delayed ()) x`.
- The type of a delayed computation is written with a leading quote: `'Nat`, or `'{IO, Exception} Text` when it uses [abilities](learn:lang/abilities).
- You pass one wherever a function runs code later, or decides whether to run it: `test.verify do ...` (see [testing](learn:testing/syntax)), `Random.listOf (do Random.nat()) (do Random.natIn 0 20)`, `catch do riskyThing()`.

## Detail

**An example.**

````
``` unison
delayed : 'Nat
delayed = do 5

add : Nat -> Nat -> Nat
add a b = a + b

> add delayed() 2
```
````

This gives `7`.

**Functions that return a delayed computation.** `f x` is a function call that gives back a delayed computation, and `f x ()` forces it.

**Why.** An [ability](learn:lang/abilities)'s effects happen when a computation runs, so a program that does IO is a delayed computation, which `run` forces (see [running code](learn:ucm/run)).
