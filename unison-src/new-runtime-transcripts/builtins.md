# Unit tests for builtin functions

```ucm:hide
.> builtins.merge
```
```unison:hide
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

```ucm:hide
.> add
```

## Bit twiddling functions

```unison:hide
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

```ucm:hide
.> add
```

## Run the tests

```ucm
.> test
```
