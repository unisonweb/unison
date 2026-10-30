```` unison
slowSquares.docs.examples.small : '{IO, Exception} [Nat]
slowSquares.docs.examples.small = do Threads.run do slowSquares [1, 2, 3]

slowSquares.doc = {{
{{ docExample 1 (do (xs -> slowSquares xs)) }} returns the squares of `xs`, in the same order. Each square takes 200 milliseconds to compute, and they are computed concurrently, so a list of 20 takes about 200 milliseconds rather than 4 seconds. It needs the `IO`, `Exception` and `Threads` abilities, so run it inside {IO.Threads.run}. For example:

@source{slowSquares.docs.examples.small}

Run it in UCM with:

```raw
run slowSquares.docs.examples.small
```

which returns

@typecheck ```
[1, 4, 9]
```

*Also see:* {IO.Threads.parMap}
}}

slowSquares : [Nat] ->{IO, Exception, Threads} [Nat]
slowSquares xs =
  slowSquare n =
    Threads.sleep (Duration.milliseconds +200)
    n Nat.* n
  Threads.parMap slowSquare xs

slowSquares.tests.example : '{IO, Exception} [Result]
slowSquares.tests.example = do test.verify do
  ensureEqual (Threads.run do slowSquares [1, 2, 3]) [1, 4, 9]
  ensureEqual (Threads.run do Threads.timeout (Duration.seconds +2) do slowSquares (List.range 0 20)) (List.map (n -> n Nat.* n) (List.range 0 20))
  ensureEqual (Threads.run do slowSquares []) []
````
``` ucm
scratch/main> add
scratch/main> docs slowSquares
scratch/main> io.test slowSquares.tests.example
```
