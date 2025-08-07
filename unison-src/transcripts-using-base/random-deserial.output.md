``` unison
directory = "unison-src/transcripts-using-base/serialized-cases/"

availableCases : '{IO,Exception} [Text]
availableCases _ =
  l = filter (contains ".ser") (directoryContents directory)
  map (t -> Text.take (drop (Text.size t) 7) t) l

gen : Nat -> Nat -> (Nat, Nat)
gen seed k =
  c = 1442695040888963407
  a = 6364136223846793005
  (mod seed k, a * seed + c)

shuffle0 : Nat -> [a] -> [a]
shuffle0 =
  pick acc seed = cases
    l | lteq (List.size l) 1 -> acc ++ l
      | otherwise -> match gen seed (size l) with
        (k, seed) -> match (take k l, drop k l) with
          (pre, x +: post) -> pick (acc :+ x) seed (pre ++ post)
          (pre, []) -> pick acc seed pre

  pick []

shuffle : [a] -> [a]
shuffle xs = shuffle0 (toRepresentation !systemTimeMicroseconds) xs

collectFailures : Text -> Nat -> Text ->{Exception, IO} [Text]
collectFailures name version target =
  vname = name ++ ".v" ++ toText version
  sfile = directory ++ vname ++ ".ser"
  hfile = directory ++ vname ++ ".hash"

  Stream.toList do
    when (fileExists sfile) do
      p@(f, i) = loadSelfContained sfile
      when (not (f i == target)) do
        emit (vname ++ " output mismatch")
      when (fileExists hfile) do
        h = readFile hfile
        when (not (h == toBase32 (crypto.hash Sha3_512 p))) do
          emit (vname ++ " hash mismatch")

runTestCase : Text ->{Exception,IO} (Text, [Test.Result])
runTestCase name =
  ofile = directory ++ name ++ ".out"

  target = fromUtf8 (readFile ofile)

  test : Nat -> (Nat, [Text])
  test ver = (ver, collectFailures name ver target)

  failures : [(Nat,[Text])]
  failures = bSort (List.map test (shuffle [3, 4, 5]))

  result : (Nat, [Text]) -> [Test.Result]
  result = cases
    (ver, []) -> [Ok (name ++ " v" ++ toText ver)]
    (_, fails) -> List.map Fail fails

  (name, foldMap result failures)

serialTests : '{IO,Exception} [Test.Result]
serialTests = do
  l = !availableCases
  cs = shuffle l
  List.foldMap snd (bSort (List.map runTestCase cs))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + availableCases  : '{IO, Exception} [Text]
  + collectFailures : Text
                      -> Nat
                      -> Text
                      ->{IO, Exception} [Text]
  + directory       : Text
  + gen             : Nat -> Nat -> (Nat, Nat)
  + runTestCase     : Text ->{IO, Exception} (Text, [Result])
  + serialTests     : '{IO, Exception} [Result]
  + shuffle         : [a] ->{IO} [a]
  + shuffle0        : Nat -> [a] -> [a]

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> io.test serialTests

    New test results:

    1. serialTests   ◉ case-00 v3
                     ◉ case-00 v4
                     ◉ case-00 v5
                     ◉ case-01 v3
                     ◉ case-01 v4
                     ◉ case-01 v5
                     ◉ case-02 v3
                     ◉ case-02 v4
                     ◉ case-02 v5
                     ◉ case-03 v3
                     ◉ case-03 v4
                     ◉ case-03 v5
                     ◉ case-04 v3
                     ◉ case-04 v4
                     ◉ case-04 v5

  ✅ 15 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
