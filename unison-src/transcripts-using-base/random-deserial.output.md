```unison
directory = "unison-src/transcripts-using-base/serialized-cases/"

availableCases : '{IO,Exception} [Text]
availableCases _ =
  l = filter (contains ".ser") (directoryContents directory)
  map (t -> Text.take (drop (Text.size t) 4) t) l

gen : Nat -> Nat -> (Nat, Nat)
gen seed k =
  c = 1442695040888963407
  a = 6364136223846793005
  (mod seed k, a * seed + c)
  
shuffle : Nat -> [a] -> [a]
shuffle =
  pick acc seed = cases
    l | lteq (List.size l) 1 -> acc ++ l
      | otherwise -> match gen seed (size l) with
        (k, seed) -> match (take k l, drop k l) with
          (pre, x +: post) -> pick (acc :+ x) seed (pre ++ post)
  
  pick []

runTestCase : Text ->{Exception,IO} (Text, Test.Result)
runTestCase name =
  sfile = directory ++ name ++ ".ser"
  ofile = directory ++ name ++ ".out"
  hfile = directory ++ name ++ ".hash"

  p@(f, i) = loadSelfContained sfile
  o = fromUtf8 (readFile ofile)
  h = readFile hfile

  result =
    if f i == o
    then if crypto.hash Sha3_512 p == h
         then Ok name
         else Fail (name ++ " hash mismatch")
    else Fail (name ++ " output mismatch")
  (name, result)
  
serialTests : '{IO,Exception} [Test.Result]
serialTests = do
  l = !availableCases
  cs = shuffle (toRepresentation !systemTimeMicroseconds) l
  List.map snd (bSort (List.map runTestCase cs))
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      availableCases : '{IO, Exception} [Text]
      directory      : Text
      gen            : Nat -> Nat -> (Nat, Nat)
      runTestCase    : Text ->{IO, Exception} (Text, Result)
      serialTests    : '{IO, Exception} [Result]
      shuffle        : Nat -> [a] -> [a]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    availableCases : '{IO, Exception} [Text]
    directory      : Text
    gen            : Nat -> Nat -> (Nat, Nat)
    runTestCase    : Text ->{IO, Exception} (Text, Result)
    serialTests    : '{IO, Exception} [Result]
    shuffle        : Nat -> [a] -> [a]

.> io.test serialTests

    New test results:
  
  ◉ serialTests   case-00
  ◉ serialTests   case-01
  ◉ serialTests   case-02
  ◉ serialTests   case-03
  
  ✅ 4 test(s) passing
  
  Tip: Use view serialTests to view the source of a test.

```
