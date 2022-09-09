# Definition Summary APIs

```ucm:hide
.> builtins.mergeio
```


```unison:hide
int = 42
doc = {{ Hello }}
test> mytest = [Test.Result.Ok "ok"]
func x = x ++ "hello"

structural type Thing = This Nat | That
```

```ucm:hide
.> add
```

## Term Summary APIs

```api
-- term
GET /api/definitions/terms/by_name/int@qkhkl0n238/summary

-- doc
GET /api/definitions/terms/by_name/doc@icfnhas71n/summary

-- test
GET /api/definitions/terms/by_name/mytest@u17p9803hd/summary

-- function
GET /api/definitions/terms/by_name/func@6ee6j48hk3/summary

-- constructor
GET /api/definitions/terms/by_name/Thing.This@altimqs66j@0/summary
```
