```ucm
.> builtins.merge
```

```unison /private/tmp/scratch.u
foo = 123

bar = 456

mytest = [Ok "ok"]
```

```ucm
.> add
.> edit foo bar
.> edit mytest
```

```ucm:error
.> edit missing
```
