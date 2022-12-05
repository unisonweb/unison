```ucm:hide
.> pull unison.public.base.releases.M4d base
```

```unison:hide
benchmarkFilePath = FilePath "unison-src/transcripts-manual/benchmarks/output.bench.txt"

timeit : Text -> '{IO,Exception} a ->{IO,Exception} a
timeit label a = 
  before = !realtime
  r = !a
  after = !realtime
  elapsed = Duration.between before after
  elapsedText = Duration.toText elapsed
  go file = 
    putText file ("\n" ++ Int.toText (Duration.countMicroseconds elapsed) ++ "   # " ++ elapsedText)
    printLine ("\n\n ******** \n")
    printLine (label ++ " took " ++ elapsedText)
  bracket '(FilePath.open benchmarkFilePath FileMode.Append) Handle.close go
  r

prepare = do 
  -- if benchmarkFilePath exists, move it to blah-<datetime>.txt for archive purposes 
  use Text ++
  if FilePath.exists benchmarkFilePath then
    now = OffsetDateTime.toText (atUTC !realtime)
    timestamped = FilePath.toText benchmarkFilePath ++ "-" ++ now ++ "-bench.txt" 
    renameFile benchmarkFilePath (FilePath timestamped)
  else 
    ()
```

```ucm:hide
.> add
.> run prepare
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/simpleloop.u
.> run main
```
