
```ucm:hide
.> builtins.merge
.> pull unison.public.base.latest.IO
.> pull dolio.public.internal.trunk.compiler
```

```unison
printHello = '(printLine "Hello")

schemeToFile dest link = 
	fop = open (FilePath dest) Write
	text = generateScheme false link
	putText fop text
	close fop
```

```ucm
.> add
```

```unison
test1 = '(schemeToFile "test-1.ss" (termLink printHello))
```

```ucm
.> run test1
```

Now run the following:
```bash
$ scheme --libdirs ../:~/.cache/unisonlanguage/scheme-libs/ --script test-1.ss
```
