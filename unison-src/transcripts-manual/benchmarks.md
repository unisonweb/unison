```ucm:hide
.> pull unison.public.base.releases.M4d base
.> pull runarorama.public.sort.data sort
```

```unison:hide
benchmarkFilePath = FilePath "unison-src/transcripts-manual/benchmarks/output.bench.txt"
archiveFilePath = FilePath "unison-src/transcripts-manual/benchmarks/output"

timeit : Text -> '{IO,Exception} a ->{IO,Exception} a
timeit label a = 
  before = !realtime
  r = !a
  after = !realtime
  elapsed = Duration.between before after
  elapsedText = Duration.toText elapsed
  go file = 
    putText file ("\n" ++ label ++ " " ++ Int.toText (Duration.countMicroseconds elapsed) ++ "   # " ++ elapsedText)
    printLine ("\n\n ******** \n")
    printLine (label ++ " took " ++ elapsedText)
  bracket '(FilePath.open benchmarkFilePath FileMode.Append) Handle.close go
  r

prepare = do 
  -- if benchmarkFilePath exists, move it to blah-<datetime>.txt for archive purposes 
  use Text ++
  if FilePath.exists benchmarkFilePath then
    createDirectory archiveFilePath
    now = OffsetDateTime.toText (atUTC !realtime)
    timestamped = FilePath.toText archiveFilePath ++ "/" ++ now ++ "-bench.txt" 
    renameFile benchmarkFilePath (FilePath timestamped)
  else 
    ()
```

```ucm:hide
.> add
.> run prepare
```

## Benchmarks

```ucm
.> load unison-src/transcripts-manual/benchmarks/each.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/listmap.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/listfilter.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/random.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/simpleloop.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/fibonacci.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/map.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/natmap.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/stm.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/tmap.u
.> run main
```

```ucm
.> load unison-src/transcripts-manual/benchmarks/array-sort.u
.> run main
```

-- something with arrays
