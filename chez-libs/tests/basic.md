
```ucm:hide
.> builtins.merge
.> pull unison.public.base.latest.Bytes
.> pull unison.public.base.latest.IO
.> pull dolio.public.internal.trunk.compiler
```

```unison
printHello = '(printLine "Hello")

generateBaseFiles _ =
	h = open (FilePath "unison/builtin-generated.ss") Write
	putText h (generateBaseFile builtinSpec)
	close h

schemeToFile dest link = 
	h = open (FilePath dest) Write
	text = generateScheme true link
	putText h text
	close h
```

```ucm:hide
.> add
.> run generateBaseFiles
```

```unison
test1 = '(schemeToFile "test-1.ss" (termLink printHello))
```

```ucm
.> run test1
```

Now run the following:
```bash
$ scheme --libdirs ../:./ --script test-1.ss
```

```unison
printBytes _ = printLine (toHex (Bytes.fromList [100, 200, 16]))
```

```ucm:hide
.> add
```

```unison
test2 = '(schemeToFile "test-2.ss" (termLink printBytes))
```

```ucm
.> run test2
```

Now run the following:
```bash
$ scheme --libdirs ../:./ --script test-2.ss
```


