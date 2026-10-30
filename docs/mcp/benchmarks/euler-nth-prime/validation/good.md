``` unison
nthPrime : Nat -> Nat
nthPrime n =
  isPrime primes c = List.all (p -> Nat.mod c p !== 0) (List.takeWhile (p -> p * p <= c) primes)
  go primes c =
    if List.size primes === n then Optional.getOrBug "no primes" (List.last primes)
    else if isPrime primes c then go (primes :+ c) (c + 1)
    else go primes (c + 1)
  go [] 2

test> nthPrime.tests.example = test.verify do
  ensureEqual (nthPrime 1) 2
  ensureEqual (nthPrime 2) 3
  ensureEqual (nthPrime 6) 13
  ensureEqual (nthPrime 25) 97
  ensureEqual (nthPrime 100) 541
```
``` ucm
scratch/main> add
scratch/main> test
```
