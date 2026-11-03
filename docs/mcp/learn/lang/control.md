If/else, boolean operators, and doing things repeatedly, which is done with recursion and with list functions, not loops.

- `if condition then a else b`. The condition is a `Boolean`. Chain with `else if`.
- `&&` and `||` skip their right side when the left side decides the result. `Boolean.not` negates.
- There are no `for` or `while` loops. To process a list, use `List.map`, `List.filter`, `List.foldLeft`, or write a recursive function that [matches](learn:lang/cases) a list.
- A recursive call in tail position, the last thing a function does, doesn't grow the stack, so loops can be written with a helper that carries an accumulator.

## Detail

**A recursive function and a tail-recursive one.**

````
``` unison
total : [Nat] -> Nat
total = cases
  [] -> 0
  h +: t -> h Nat.+ total t

totalLoop : [Nat] -> Nat
totalLoop xs =
  use Nat +
  go acc = cases
    [] -> acc
    h +: t -> go (acc + h) t
  go 0 xs
```
````

**Stopping early.** There is no `break`. Use `List.takeWhile` or `List.filter` and then `List.map`.

**Effects in a loop.** `List.map` accepts a function that uses [abilities](learn:lang/abilities), and passes its abilities through.
