
```ucm:hide
.> builtins.merge
.> pull unison.public.base.latest.IO
.> pull dolio.public.internal.trunk.compiler
```

```unison
printHello = '(printLine "Hello")

schemeToFile dest link = 
	fop = open (FilePath dest) Write
	text = generateScheme true link
	putText fop text
	close fop
```

```ucm:hide
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

```unison
printBytes = printLine (toHexString (base.Bytes.fromList [100, 200, 16]))
```

```ucm:hide
.> add
```

```unison
test2 = '(schemeToFile "test-2.ss" (termLink test2))
```

```ucm
.> run test2
```

Now run the following:
```bash
$ scheme --libdirs ../:~/.cache/unisonlanguage/scheme-libs/ --script test-2.ss
```


