
```unison
printHello = '(printLine "Hello")

generateBaseFiles _ =
	h = open (FilePath "bootSpec.ss") Write
	putText h (generateBaseFile bootSpec)
	close h
	h2 = open (FilePath "builtinSpec.ss") Write
	putText h2 (generateBaseFile builtinSpec)
	close h2

schemeToFile dest link = 
	h = open (FilePath dest) Write
	text = generateScheme true link
	putText h text
	close h
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      generateBaseFiles : ∀ _. _ ->{IO, Exception} ()
      printHello        : '{IO, Exception} ()
      schemeToFile      : Text -> Term ->{IO, Exception} ()

```
```unison
test1 = '(schemeToFile "test-1.ss" (termLink printHello))
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test1 : '{IO, Exception} ()

```
```ucm
.> run test1

  ()

```
Now run the following:
```bash

$ scheme --libdirs ../:~/.cache/unisonlanguage/scheme-libs/ --script test-1.ss

```

```unison
printBytes _ = printLine (toHex (Bytes.fromList [100, 200, 16]))
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      printBytes : ∀ _. _ ->{IO, Exception} ()

```
```unison
test2 = '(schemeToFile "test-2.ss" (termLink printBytes))
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test2 : '{IO, Exception} ()

```
```ucm
.> run test2

  ()

```
Now run the following:
```bash

$ scheme --libdirs ../:~/.cache/unisonlanguage/scheme-libs/ --script test-2.ss

```

