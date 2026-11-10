You are judging `factorialDigitSum` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view factorialDigitSum`: the type is `Nat -> Nat`.
2. `factorialDigitSum 10` is `27`, `factorialDigitSum 0` is `1`, `factorialDigitSum 1` is `1`.
3. `factorialDigitSum 25` is `72` (`25! = 15511210043330985984000000`; a solution using 64-bit `Nat` overflows here and gives a different answer).
4. `factorialDigitSum 100` is `648`.
5. `factorialDigitSum 1000` is `10539`.

`factorialDigitSum` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
