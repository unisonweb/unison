``` unison
slowSquares : [Nat] ->{IO, Exception, Threads} [Nat]
slowSquares xs =
  slowSquare n =
    Threads.sleep (Duration.milliseconds +200)
    n Nat.* n
  Threads.scope do
    tasks = List.map (n -> Threads.fork (do slowSquare n)) xs
    List.map (t -> t()) tasks

slowSquares.tests.example : '{IO, Exception} [Result]
slowSquares.tests.example = do test.verify do
  ensureEqual (Threads.run do slowSquares [1, 2, 3]) [1, 4, 9]
  ensureEqual (Threads.run do Threads.timeout (Duration.seconds +2) do slowSquares (List.range 0 20)) (List.map (n -> n Nat.* n) (List.range 0 20))
  ensureEqual (Threads.run do slowSquares []) []
```
``` ucm
scratch/main> add
scratch/main> io.test slowSquares.tests.example
```
