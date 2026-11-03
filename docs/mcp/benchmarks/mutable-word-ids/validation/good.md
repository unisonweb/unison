``` unison
wordIds : [Text] -> [Nat]
wordIds words = Scope.run do
  seen = Scope.ref (Map.empty : Map Text Nat)
  idOf word =
    m = Ref.read seen
    match Map.get word m with
      Some n -> n
      None ->
        n = Map.size m
        Ref.write seen (Map.insert word n m)
        n
  List.map idOf words

test> wordIds.tests.examples = test.verify do
  ensureEqual (wordIds ["a", "b", "a", "c", "b"]) [0, 1, 0, 2, 1]
  ensureEqual (wordIds ["x", "x", "x"]) [0, 0, 0]
  ensureEqual (wordIds ["c", "b", "a"]) [0, 1, 2]
  ensureEqual (wordIds []) []
```
``` ucm
scratch/main> add
scratch/main> test
```
