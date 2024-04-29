The `alias.many` command can be used to copy definitions from the current namespace into your curated one.
The names that will be used in the target namespace are the names you specify, relative to the current namespace:

```
.> help alias.many

  alias.many (or copy)
  `alias.many <relative1> [relative2...] <namespace>` creates aliases `relative1`, `relative2`, ...
  in the namespace `namespace`.
  `alias.many foo.foo bar.bar .quux` creates aliases `.quux.foo.foo` and `.quux.bar.bar`.

```

Let's try it!

```ucm
.> alias.many stuff.List.adjacentPairs stuff.List.all stuff.List.any stuff.List.chunk stuff.List.chunksOf stuff.List.dropWhile stuff.List.first stuff.List.init stuff.List.intersperse stuff.List.isEmpty stuff.List.last stuff.List.replicate stuff.List.splitAt stuff.List.tail stuff.List.takeWhile .mylib

  Here's what changed in .mylib :
  
  Added definitions:
  
    1.  stuff.List.adjacentPairs : [a] -> [(a, a)]
    2.  stuff.List.all           : (a ->{g} Boolean)
                                 -> [a]
                                 ->{g} Boolean
    3.  stuff.List.any           : (a ->{g} Boolean)
                                 -> [a]
                                 ->{g} Boolean
    4.  stuff.List.chunk         : Nat -> [a] -> [[a]]
    5.  stuff.List.chunksOf      : Nat -> [a] -> [[a]]
    6.  stuff.List.dropWhile     : (a ->{g} Boolean)
                                 -> [a]
                                 ->{g} [a]
    7.  stuff.List.first         : [a] -> Optional a
    8.  stuff.List.init          : [a] -> Optional [a]
    9.  stuff.List.intersperse   : a -> [a] -> [a]
    10. stuff.List.isEmpty       : [a] -> Boolean
    11. stuff.List.last          : [a] -> Optional a
    12. stuff.List.replicate     : Nat -> a -> [a]
    13. stuff.List.splitAt       : Nat -> [a] -> ([a], [a])
    14. stuff.List.tail          : [a] -> Optional [a]
    15. stuff.List.takeWhile     : (a ->{𝕖} Boolean)
                                 -> [a]
                                 ->{𝕖} [a]
  
  Tip: You can use `undo` or `reflog` to undo this change.

.> find-in mylib

  1.  stuff.List.adjacentPairs : [a] -> [(a, a)]
  2.  stuff.List.all : (a ->{g} Boolean) -> [a] ->{g} Boolean
  3.  stuff.List.any : (a ->{g} Boolean) -> [a] ->{g} Boolean
  4.  stuff.List.chunk : Nat -> [a] -> [[a]]
  5.  stuff.List.chunksOf : Nat -> [a] -> [[a]]
  6.  stuff.List.dropWhile : (a ->{g} Boolean) -> [a] ->{g} [a]
  7.  stuff.List.first : [a] -> Optional a
  8.  stuff.List.init : [a] -> Optional [a]
  9.  stuff.List.intersperse : a -> [a] -> [a]
  10. stuff.List.isEmpty : [a] -> Boolean
  11. stuff.List.last : [a] -> Optional a
  12. stuff.List.replicate : Nat -> a -> [a]
  13. stuff.List.splitAt : Nat -> [a] -> ([a], [a])
  14. stuff.List.tail : [a] -> Optional [a]
  15. stuff.List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
  

```
Thanks, `alias.many!
