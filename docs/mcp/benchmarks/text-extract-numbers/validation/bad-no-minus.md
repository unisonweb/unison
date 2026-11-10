``` unison
Text.numbers : Text -> [Int]
Text.numbers t =
  p = Pattern.many (Pattern.or (Pattern.capture (Pattern.some Text.patterns.digit)) Text.patterns.anyChar)
  List.filterMap Int.fromText (Pattern.captures p t)

test> Text.numbers.tests.example = test.verify do
  ensureEqual (Text.numbers "a1 b22, c333") [+1, +22, +333]
  ensureEqual (Text.numbers "x12y 7") [+12, +7]
  ensureEqual (Text.numbers "no digits here") []
  ensureEqual (Text.numbers "") []
```
``` ucm
scratch/main> add
scratch/main> test
```
