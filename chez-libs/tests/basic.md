
```ucm:hide
.> builtins.merge
.> pull unison.public.base.latest.IO base.IO
.> pull unison.public.base.main.IO.Process base.IO.Process
.> pull dolio.public.internal.trunk.compiler
```

```unison
generateSchemeBuiltinLibrary _ =
	fh = open (FilePath "../unison/builtin-generated.ss") Write
	putText fh (generateBaseFile builtinSpec)
	close fh

schemeToFile dest link = 
	fh = open (FilePath dest) Write
	putText fh (generateScheme false link)
	close fh

runChez fileName = IO.Process.call "scheme" ["--libdirs", "../", "--script", fileName]

runInScheme id term =
	fileName = "basic-" ++ (Nat.toText id) ++ ".ss"
	schemeToFile fileName term
	runChez fileName
```

```ucm
.> add
.> run generateSchemeBuiltinLibrary
```

```unison
test1_term = '(printLine "Hello")
```

```ucm:hide
.> add
```

```unison
test1 = '(runInScheme 1 (termLink test1_term))
```

```ucm
.> run test1
```
