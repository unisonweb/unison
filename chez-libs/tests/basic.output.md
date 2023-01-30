
```unison
generateSchemeBuiltinLibrary _ =
	fh = open (FilePath "../unison/builtin-generated.ss") Write
	putText fh (generateBaseFile builtinSpec)
	close fh

schemeToFile dest link = 
	fh = open (FilePath dest) Write
	putText fh (generateScheme false link)
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
	(stdin, stdout, stderr, pid) = IO.Process.start "scheme" ["--libdirs", "../:./", "--script", fileName]
	exitCode = match wait pid with
		0 -> ""
		code -> "Non-zero exit code! " ++ (toText code) ++ "\n"
	exitCode ++ readAll stdout ++ readAll stderr

runInScheme id term =
	fileName = "basic-" ++ (Nat.toText id) ++ ".ss"
	schemeToFile fileName term
	runChez fileName
```

```unison
test1_term = '(printLine "Hello")
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test1_term : '{IO, Exception} ()

```
```unison
test1 = '(runInScheme 1 (termLink test1_term))
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test1 : '{IO, Exception} Text

```
```ucm
.> run test1

  ""

```
