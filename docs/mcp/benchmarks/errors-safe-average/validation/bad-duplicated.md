```` unison
average.doc = {{
{{ docExample 1 (do (xs -> average xs)) }} is the integer average of `xs`: the sum divided by the length, with integer division. It fails with an `Exception` if `xs` is empty. For example:

```
catch do average [+2, +4, +6]
```

*Also see:* {averageOrNone}
}}

averageOrNone.doc = {{
{{ docExample 1 (do (xs -> averageOrNone xs)) }} is `Some` of the integer average of `xs`, or `None` if `xs` is empty. For example:

```
averageOrNone [+3, +4]
```

```
averageOrNone []
```

*Also see:* {average}
}}

average : [Int] ->{Exception} Int
average xs =
  if List.isEmpty xs then Exception.raiseGeneric "average of an empty list" xs
  else List.foldLeft (Int.+) +0 xs / Nat.toInt (List.size xs)

averageOrNone : [Int] -> Optional Int
averageOrNone xs =
  if List.isEmpty xs then None
  else Some (List.foldLeft (Int.+) +0 xs / Nat.toInt (List.size xs))

test> average.tests.example = test.verify do
  ensureEqual (Exception.catch do average [+2, +4, +6]) (Right +4)
  ensureEqual (Exception.catch do average [+7]) (Right +7)
  ensureEqual (averageOrNone [+3, +4]) (Some +3)
  ensureEqual (averageOrNone []) None
  ensure (Either.isLeft (Exception.catch do average []))
````
``` ucm
scratch/main> add
scratch/main> test
```
