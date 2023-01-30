
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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      generateSchemeBuiltinLibrary : ∀ _. _ ->{IO, Exception} ()
      runChez                      : Text ->{IO} Nat
      runInScheme                  : Nat
                                     -> Term
                                     ->{IO, Exception} Nat
      schemeToFile                 : Text
                                     -> Term
                                     ->{IO, Exception} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    generateSchemeBuiltinLibrary : ∀ _. _ ->{IO, Exception} ()
    runChez                      : Text ->{IO} Nat
    runInScheme                  : Nat
                                   -> Term
                                   ->{IO, Exception} Nat
    schemeToFile                 : Text
                                   -> Term
                                   ->{IO, Exception} ()

.> run generateSchemeBuiltinLibrary

  ()

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
    
      test1 : '{IO, Exception} Nat

```
```ucm
.> run test1

  0

```
