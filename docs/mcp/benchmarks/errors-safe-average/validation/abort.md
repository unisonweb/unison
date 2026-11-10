```` unison
average.doc = {{
{{ docExample 1 (do (xs -> average xs)) }} is the integer average of `xs`: the sum divided by the length, with integer division. It aborts if `xs` is empty. For example:

```
toOptional! do average [+2, +4, +6]
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

average : [Int] ->{Abort} Int
average xs =
  if List.isEmpty xs then Abort.abort
  else List.foldLeft (Int.+) +0 xs / Nat.toInt (List.size xs)

averageOrNone : [Int] -> Optional Int
averageOrNone xs = Abort.toOptional! do average xs

test> average.tests.example = test.verify do
  ensureEqual (Abort.toOptional! do average [+2, +4, +6]) (Some +4)
  ensureEqual (averageOrNone [+3, +4]) (Some +3)
  ensureEqual (averageOrNone []) None
  ensureEqual (Abort.toOptional! do average []) None
````
``` ucm
scratch/main> add
scratch/main> test
```
