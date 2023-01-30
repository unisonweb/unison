
```ucm
.> builtins.merge

  Done.

.> pull unison.public.base.latest.IO base.IO

  😶
  
  base.IO was already up-to-date with
  unison.public.base.latest.IO.

.> pull unison.public.base.main.IO.Process base.IO.Process

  😶
  
  base.IO.Process was already up-to-date with
  unison.public.base.main.IO.Process.

.> pull dolio.public.internal.trunk.compiler

  😶
  
  the current namespace was already up-to-date with
  dolio.public.internal.trunk.compiler.

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

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      generateSchemeBuiltinLibrary : ∀ _. _ ->{IO, Exception} ()
      orDefault                    : a -> Optional a -> a
      readAll                      : Handle
                                     ->{IO, Exception} Text
      right                        : Either a b -> Optional b
      runChez                      : Text ->{IO, Exception} Text
      runInScheme                  : Nat
                                     -> Term
                                     ->{IO, Exception} Text
      schemeToFile                 : Text
                                     -> Term
                                     ->{IO, Exception} ()
      |>                           : a -> (a ->{g} t) ->{g} t

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    generateSchemeBuiltinLibrary : ∀ _. _ ->{IO, Exception} ()
    orDefault                    : a -> Optional a -> a
    readAll                      : Handle ->{IO, Exception} Text
    right                        : Either a b -> Optional b
    runChez                      : Text ->{IO, Exception} Text
    runInScheme                  : Nat
                                   -> Term
                                   ->{IO, Exception} Text
    schemeToFile                 : Text
                                   -> Term
                                   ->{IO, Exception} ()
    |>                           : a -> (a ->{g} t) ->{g} t

.> run generateSchemeBuiltinLibrary

  ()

```
