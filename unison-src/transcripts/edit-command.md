``` ucm
scratch/main> builtins.merge
```

``` unison /private/tmp/scratch.u
foo = 123

bar = 456

mytest = [Ok "ok"]
```

``` ucm
scratch/main> add
scratch/main> edit foo bar
scratch/main> edit mytest
```

``` ucm :error
scratch/main> edit missing
```
