# Ensure Test watch dependencies are properly considered.

https://github.com/unisonweb/unison/issues/2195

```ucm:hide
.> builtins.merge
```

```unison
x = 999
```

```ucm
.> add
```

```unison
x = 1000
test> mytest = [let x + 1 == 1001; Ok "ok"]
```

```ucm
.> add
```
