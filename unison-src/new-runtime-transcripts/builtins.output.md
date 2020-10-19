# Unit tests for builtin functions

```unison
check : Boolean -> [Result]
check b = if b then [Ok "Passed"] else [Fail "Failed"]

checks : [Boolean] -> [Result]
checks = cases
  [] -> [Ok "Passed"]
  h +: t -> check h ++ checks t

map : (a ->{m} b) -> [a] ->{m} [b]
map f xs =
  go acc = cases
    [] -> acc
    h +: t -> go (acc :+ f h) t
  go [] xs
```

## Bit twiddling functions

```unison
test> Nat.popCount.test.ex1 = check (Nat.popCount 1 == 1)
test> Nat.popCount.test.ex2 = check (Nat.popCount 2 == 1)
test> Nat.popCount.test.ex3 = check (Nat.popCount 4 == 1)
test> Nat.popCount.test.ex4 = check (Nat.popCount 5 == 2)
test> Int.popCount.test.ex1 = check (Int.popCount +1 == 1)
test> Int.popCount.test.ex2 = check (Int.popCount +2 == 1)
test> Int.popCount.test.ex3 = check (Int.popCount +4 == 1)
test> Int.popCount.test.ex4 = check (Int.popCount +5 == 2)
test> Int.popCount.test.ex5 = check (Int.popCount -1 == 64)
```

## Run the tests

```ucm
.> test

  Cached test results (`help testcache` to learn more)
  
  ◉ Int.popCount.test.ex1   Passed
  ◉ Int.popCount.test.ex2   Passed
  ◉ Int.popCount.test.ex3   Passed
  ◉ Int.popCount.test.ex4   Passed
  ◉ Int.popCount.test.ex5   Passed
  ◉ Nat.popCount.test.ex1   Passed
  ◉ Nat.popCount.test.ex2   Passed
  ◉ Nat.popCount.test.ex3   Passed
  ◉ Nat.popCount.test.ex4   Passed
  
  ✅ 9 test(s) passing
  
  Tip: Use view Int.popCount.test.ex1 to view the source of a
       test.

```
