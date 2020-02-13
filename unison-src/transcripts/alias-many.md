```ucm:hide
.> builtins.merge
```
```unison:hide:all
List.adjacentPairs : [a] -> [(a, a)]
List.adjacentPairs as =
  go xs acc =
    case xs of
      [x, y] ++ t -> go t (acc :+ (x, y))
      _ -> acc
  go as []

List.all : (a -> Boolean) -> [a] -> Boolean
List.all p xs =
  case xs of
    [] -> true
    x +: xs -> (p x) && (List.all p xs)

List.any : (a -> Boolean) -> [a] -> Boolean
List.any p xs =
  case xs of
    [] -> false
    x +: xs -> (p x) || (List.any p xs)

List.chunk : Nat -> [a] -> [[a]]
List.chunk n as =
  go acc rest =
    case splitAt n rest of
      (c, []) -> acc :+ c
      (c, cs) -> go (acc :+ c) cs
  go [] as

List.chunksOf : Nat -> [a] -> [[a]]
List.chunksOf n text =
  go acc text =
    p = splitAt n text
    case p of
      ([], _) -> acc
      (a, b) -> go (acc :+ a) b
  go [] text

List.dropWhile : (a -> Boolean) -> [a] -> [a]
List.dropWhile p xs =
  case xs of
    i +: l -> if p i then List.dropWhile p l else xs
    _ -> []

List.first : [a] -> Optional a
List.first a = List.at 0 a

List.init : [a] -> Optional [a]
List.init as =
  case as of
    [] -> None
    as :+ _ -> Some as

List.intersperse : a -> [a] -> [a]
List.intersperse a as =
  go acc as =
    case as of
      [] -> acc
      [x] -> acc :+ x
      x +: xs -> go (acc :+ x :+ a) xs
  go [] as

List.isEmpty : [a] -> Boolean
List.isEmpty as = List.size as == 0

List.last : [a] -> Optional a
List.last as =
  case as of
    [] -> None
    _ :+ a -> Some a

List.replicate : Nat -> a -> [a]
List.replicate n a =
  go n acc = if n == 0 then acc else go (Nat.drop n 1) (a +: acc)
  go n []

List.splitAt : Nat -> [a] -> ([a], [a])
List.splitAt n as = (List.take n as, List.drop n as)

List.tail : [a] -> Optional [a]
List.tail as =
  case as of
    [] -> None
    _ +: as -> Some as

List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
List.takeWhile p xs =
  go xs acc =
    case xs of
      x +: xs -> if p x then go xs (acc :+ x) else acc
      _ -> acc
  go xs []
```
```ucm:hide
.runar> add
```

The `alias.many` command can be used to copy definitions from the current namespace into your curated one.
The names that will be copied over are the names relative to the current namespace:

```
.> help alias.many
  alias.many (or copy)
  `alias.many foo.foo bar.bar quux` creates aliases
  `quux.foo.foo` and `quux.bar.bar`.
```

Let's try it!

```ucm
.> cd .builtin
.builtin> find
.builtin> alias.many 94-104 .mylib
```

I want to incorporate a few more from another namespace:
```ucm
.builtin> cd .runar
.runar> find
.runar> alias.many 1-15 .mylib
.runar> cd .mylib
.mylib> find
```

Thanks, `alias.many`!
