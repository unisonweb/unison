# The Haskell replacements against native code

The runtime swaps some base library definitions for Haskell at load time (the
`functionReplacementList` in `Unison.Runtime.Foreign.Function`). Under the JIT each is a
call-out. This transcript times every replaced operation on base's own definitions, so it can
be run three ways against the `jit_codebase` codebase (the `jit-tests` project with base,
`unison_json`, `printTime` and `repeat` in it), on an optimized build:

```
UNISON_JIT=off                            # the interpreter with the replacement, today's default
UNISON_JIT=on UNISON_REPLACEMENTS=on      # the replacement as a call-out from native code
UNISON_JIT=on UNISON_REPLACEMENTS=off     # base's own Unison code, compiled
stack exec --work-dir .stack-work-opt unison -- -C jit_codebase transcript.fork unison-src/transcripts-manual/jit-replacements.md
```

Each row does 100000 operations (or builds, sorts or walks 100000 elements in all), so a time
in milliseconds times ten is nanoseconds per operation; the JSON rows say their own counts. The results and the tags they decided are in
`unison-runtime/src/Unison/Runtime/JIT/benchmarks/2026-10-11-replacements.md`. The
`Avro.decodeBinary` replacement is not here: the codebase has no Avro library.

``` unison
-- keys in a scrambled order, so that the balancing runs
key : Nat -> Nat -> Nat
key n i = Nat.mod (i * 7919) n

buildMap : Nat -> Map Nat Nat
buildMap n =
  go i m = if i == n then m else go (i + 1) (Map.insert (key n i) i m)
  go 0 Map.empty

buildTextMap : Nat -> Map Text Nat
buildTextMap n =
  go i m = if i == n then m else go (i + 1) (Map.insert (Nat.toText (key n i)) i m)
  go 0 Map.empty

lookups : Nat -> Nat -> Map Nat Nat -> Nat
lookups n reps m =
  go i acc = if i == reps then acc else
    go (i + 1) (acc + Optional.getOrElse 0 (Map.get (key n i) m))
  go 0 0

textLookups : Nat -> Nat -> Map Text Nat -> Nat
textLookups n reps m =
  go i acc = if i == reps then acc else
    go (i + 1) (acc + Optional.getOrElse 0 (Map.get (Nat.toText (key n i)) m))
  go 0 0

sortedPairs : Nat -> [(Nat, Nat)]
sortedPairs n = List.map (i -> (i, i)) (List.range 0 n)

scrambledPairs : Nat -> [(Nat, Nat)]
scrambledPairs n = List.map (i -> (key n i, i)) (List.range 0 n)

-- the keys lo, lo + 1, ... lo + n - 1
shiftedMap : Nat -> Nat -> Map Nat Nat
shiftedMap lo n = Map.fromList (List.map (i -> (lo + i, i)) (List.range 0 n))

scrambled : Nat -> [Nat]
scrambled n = List.map (key n) (List.range 0 n)

scrambledTexts : Nat -> [Text]
scrambledTexts n = List.map (i -> Nat.toText (key n i)) (List.range 0 n)

-- n pairs over n / 10 distinct keys
multiPairs : Nat -> [(Nat, Nat)]
multiPairs n = List.map (i -> (Nat.mod (key n i) (Nat.max 1 (n / 10)), i)) (List.range 0 n)

smallJson : Json
smallJson =
  Json.object
    [ ("name", Json.text "Gandalf"),
      ("age", Json.nat 2019),
      ("tags", Json.array [Json.text "wizard", Json.text "grey", Json.null]),
      ("quote", Json.text "A wizard is never late,\nnor is he early.") ]

largeJson : Json
largeJson = Json.array (List.map (i -> Json.object.add "i" (Json.nat i) smallJson) (List.range 0 10000))

bytesReads : Nat -> Bytes ->{Exception} Nat
bytesReads reps b =
  go i acc = if i == reps then acc else go (i + 1) (acc + Bytes.read (Nat.mod i 900) b)
  go 0 0

bytesReads16le : Nat -> Bytes ->{Exception} Nat
bytesReads16le reps b =
  go i acc = if i == reps then acc else go (i + 1) (acc + Bytes.read16le (Nat.mod i 900) b)
  go 0 0

bytesReads16be : Nat -> Bytes ->{Exception} Nat
bytesReads16be reps b =
  go i acc = if i == reps then acc else go (i + 1) (acc + Bytes.read16be (Nat.mod i 900) b)
  go 0 0

bytesReads32le : Nat -> Bytes ->{Exception} Nat
bytesReads32le reps b =
  go i acc = if i == reps then acc else go (i + 1) (acc + Bytes.read32le (Nat.mod i 900) b)
  go 0 0

bytesReads32be : Nat -> Bytes ->{Exception} Nat
bytesReads32be reps b =
  go i acc = if i == reps then acc else go (i + 1) (acc + Bytes.read32be (Nat.mod i 900) b)
  go 0 0

bytesReads64le : Nat -> Bytes ->{Exception} Nat
bytesReads64le reps b =
  go i acc = if i == reps then acc else go (i + 1) (acc + Bytes.read64le (Nat.mod i 900) b)
  go 0 0

bytesReads64be : Nat -> Bytes ->{Exception} Nat
bytesReads64be reps b =
  go i acc = if i == reps then acc else go (i + 1) (acc + Bytes.read64be (Nat.mod i 900) b)
  go 0 0

-- every row does 100000 operations: reps rounds over n elements
atSize : Nat ->{IO, Exception} ()
atSize n =
  reps = Nat.max 1 (100000 / n)
  label = Nat.toText n
  m = buildMap n
  mt = buildTextMap n
  m2 = shiftedMap (n / 2) n
  mEq = buildMap n
  sorted = sortedPairs n
  pairs = scrambledPairs n
  nats = scrambled n
  natsSorted = List.range 0 n
  texts = scrambledTexts n
  s1 = Set.fromList nats
  s2 = Set.fromList (List.map (i -> i + n / 2) natsSorted)
  multi = multiPairs n
  printTime ("Map.insert: " ++ label ++ " Nat keys") 1 (k -> repeat (k * reps) do buildMap n)
  printTime ("Map.lookup: 100000 lookups in " ++ label ++ " Nat keys") 1 (k -> ignore (lookups n (k * 100000) m))
  printTime ("Map.insert: " ++ label ++ " Text keys") 1 (k -> repeat (k * reps) do buildTextMap n)
  printTime ("Map.lookup: 100000 lookups in " ++ label ++ " Text keys") 1 (k -> ignore (textLookups n (k * 100000) mt))
  printTime ("Map.fromList: " ++ label ++ " sorted pairs") 1 (k -> repeat (k * reps) do Map.fromList sorted)
  printTime ("Map.fromList: " ++ label ++ " scrambled pairs") 1 (k -> repeat (k * reps) do Map.fromList pairs)
  printTime ("Map.union: two maps of " ++ label ++ " keys, half overlapping") 1 (k -> repeat (k * reps) do Map.union m m2)
  printTime ("Map.intersect: two maps of " ++ label ++ " keys, half overlapping") 1 (k -> repeat (k * reps) do Map.intersect m m2)
  printTime ("Map.toList: " ++ label ++ " keys") 1 (k -> repeat (k * reps) do Map.toList m)
  printTime ("Map.==: two equal maps of " ++ label ++ " keys") 1 (k -> repeat (k * reps) do m Map.== mEq)
  printTime ("Set.fromList: " ++ label ++ " scrambled Nats") 1 (k -> repeat (k * reps) do Set.fromList nats)
  printTime ("Set.union: two sets of " ++ label ++ ", half overlapping") 1 (k -> repeat (k * reps) do Set.union s1 s2)
  printTime ("Set.intersect: two sets of " ++ label ++ ", half overlapping") 1 (k -> repeat (k * reps) do Set.intersect s1 s2)
  printTime ("Set.toList: " ++ label ++ " elements") 1 (k -> repeat (k * reps) do Set.toList s1)
  printTime ("Multimap.fromList: " ++ label ++ " pairs over " ++ Nat.toText (Nat.max 1 (n / 10)) ++ " keys") 1 (k -> repeat (k * reps) do Multimap.fromList multi)
  printTime ("List.range: 0 to " ++ label) 1 (k -> repeat (k * reps) do List.range 0 n)
  printTime ("List.sort: " ++ label ++ " sorted Nats") 1 (k -> repeat (k * reps) do List.sort natsSorted)
  printTime ("List.sort: " ++ label ++ " scrambled Nats") 1 (k -> repeat (k * reps) do List.sort nats)
  printTime ("List.sort: " ++ label ++ " scrambled Texts") 1 (k -> repeat (k * reps) do List.sort texts)

replacementSuite : '{IO, Exception} ()
replacementSuite = do
  List.foreach atSize [10, 1000, 100000]
  smallText = Json.toText smallJson
  largeText = Json.toText largeJson
  printTime ("Json.toText: a 4-field object, 10000 times (" ++ Nat.toText (Text.size smallText) ++ " characters)") 1 (k -> repeat (k * 10000) do Json.toText smallJson)
  printTime ("Json.toText: an array of 10000 objects, twice (" ++ Nat.toText (Text.size largeText) ++ " characters)") 1 (k -> repeat (k * 2) do Json.toText largeJson)
  printTime "Json.unconsText: the 4-field object, 10000 times" 1 (k -> repeat (k * 10000) do Json.unconsText smallText)
  printTime "Json.unconsText: the array of 10000 objects, twice" 1 (k -> repeat (k * 2) do Json.unconsText largeText)
  printTime "Json.tryUnconsText: the 4-field object, 10000 times" 1 (k -> repeat (k * 10000) do Json.tryUnconsText smallText)
  printTime "Json.tryUnconsText: the array of 10000 objects, twice" 1 (k -> repeat (k * 2) do Json.tryUnconsText largeText)
  b = Bytes.fromList (List.map (i -> Nat.mod i 256) (List.range 0 1000))
  printTime "Bytes.read: 100000 reads" 1 (k -> ignore (bytesReads (k * 100000) b))
  printTime "Bytes.read16le: 100000 reads" 1 (k -> ignore (bytesReads16le (k * 100000) b))
  printTime "Bytes.read16be: 100000 reads" 1 (k -> ignore (bytesReads16be (k * 100000) b))
  printTime "Bytes.read32le: 100000 reads" 1 (k -> ignore (bytesReads32le (k * 100000) b))
  printTime "Bytes.read32be: 100000 reads" 1 (k -> ignore (bytesReads32be (k * 100000) b))
  printTime "Bytes.read64le: 100000 reads" 1 (k -> ignore (bytesReads64le (k * 100000) b))
  printTime "Bytes.read64be: 100000 reads" 1 (k -> ignore (bytesReads64be (k * 100000) b))
```

``` ucm
jit-tests/main> run replacementSuite
```
