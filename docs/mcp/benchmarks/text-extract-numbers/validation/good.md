``` unison
Text.numbers : Text -> [Int]
Text.numbers t =
  p = Pattern.many (Pattern.or (Pattern.capture Text.patterns.int) Text.patterns.anyChar)
  List.filterMap Int.fromText (Pattern.captures p t)

test> Text.numbers.tests.example = test.verify do
  ensureEqual (Text.numbers "a1 b-22, c333") [+1, -22, +333]
  ensureEqual (Text.numbers "x12y 7") [+12, +7]
  ensureEqual (Text.numbers "no digits here") []
  ensureEqual (Text.numbers "") []
  ensureEqual (Text.numbers "-5") [-5]
  ensureEqual (Text.numbers "3-4") [+3, -4]
```
``` ucm
scratch/main> add
scratch/main> test
```
