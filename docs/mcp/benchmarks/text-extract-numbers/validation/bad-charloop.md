``` unison
Text.numbers : Text -> [Int]
Text.numbers t =
  isDigit c = Char.toNat c >= Char.toNat ?0 && Char.toNat c <= Char.toNat ?9
  flush cur acc = if Text.isEmpty cur then acc else acc :+ Optional.getOrBug "num" (Int.fromText cur)
  go cur acc = cases
    [] -> flush cur acc
    c +: rest ->
      if isDigit c then go (cur ++ Char.toText c) acc rest
      else if c === ?- then go "-" (flush (if cur === "-" then "" else cur) acc) rest
      else go "" (flush (if cur === "-" then "" else cur) acc) rest
  go "" [] (Text.toCharList t)

test> Text.numbers.tests.example = test.verify do
  ensureEqual (Text.numbers "a1 b-22, c333") [+1, -22, +333]
```
``` ucm
scratch/main> add
scratch/main> test
```
