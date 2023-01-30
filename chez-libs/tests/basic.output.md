
```unison
printHello = '(printLine "Hello")

schemeToFile dest link = 
	fop = open (FilePath dest) Write
	text = generateScheme true link
	putText fop text
	close fop
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      printHello   : '{IO, Exception} ()
      schemeToFile : Text -> Term ->{IO, Exception} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    printHello   : '{IO, Exception} ()
    schemeToFile : Text -> Term ->{IO, Exception} ()

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

