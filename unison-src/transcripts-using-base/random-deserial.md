```ucm:hide
.> builtins.mergeio
```

```unison:hide
directory = "unison-src/transcripts-using-base/serialized-cases/"

availableCases : '{IO,Exception} [Text]
availableCases _ =
  endsWith suffix t = Text.drop (Nat.drop (Text.size t) (Text.size suffix)) t == suffix 
  l = filter (endsWith ".ser") (directoryContents directory)
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
  sfile2 = directory ++ name ++ ".ser.small"
  ofile = directory ++ name ++ ".out"
  hfile = directory ++ name ++ ".hash"

  p@(f, i) = loadSelfContained sfile
  p2@(f2, i2) = loadSelfContained sfile2
  o = fromUtf8 (readFile ofile)
  h = readFile hfile

  result =
    if f i == o && (f2 i2 == o)
    then if toBase32 (crypto.hash Sha3_512 p) == h
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
.> add
.> io.test serialTests
```
