
```ucm
.> builtins.merge
.> pull unison.public.base.latest base
.> pull dolio.public.internal.trunk.compiler
```

```unison
generateSchemeBuiltinLibrary _ =
	fh = open (FilePath "../chez/unison/builtin-generated.ss") Write
	putText fh (generateBaseFile builtinSpec)
	close fh

schemeToFile dest link = 
	fh = open (FilePath dest) Write
	putText fh (generateScheme true link)
	close fh

a |> f = f a

right = cases
	Left _ -> None
	Right a -> Some a

orDefault a = cases
  None   -> a
  Some a -> a

readAll fid =
	getBytes fid 1024
		|> fromUtf8.impl
		|> right
		|> orDefault "Not utf8 output"

runChez fileName =
	(stdin, stdout, stderr, pid) = IO.Process.start "scheme" ["--libdirs", "../chez:../common", "--script", fileName]
	exitCode = match wait pid with
		0 -> ""
		code -> "Non-zero exit code! " ++ (Nat.toText code) ++ "\n"
	exitCode ++ readAll stdout ++ readAll stderr

runInScheme id term =
	fileName = "basic-" ++ (Nat.toText id) ++ ".ss"
	schemeToFile fileName term
	runChez fileName
```

```ucm
.> add
.> run generateSchemeBuiltinLibrary
```
