# Definition Summary APIs

```ucm:hide
.> builtins.mergeio
```


```unison:hide
nat : Nat
nat = 42
doc : Doc2
doc = {{ Hello }}
test> mytest = [Test.Result.Ok "ok"]
func : Text -> Text
func x = x ++ "hello"

funcWithLongType : Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
funcWithLongType a b c d e f g h = a ++ b ++ c ++ d ++ e ++ f ++ g ++ h

structural type Thing = This Nat | That
```

```ucm:hide
.> add
```

## Term Summary APIs

```api
-- term
GET /api/definitions/terms/by_name/nat@qkhkl0n238/summary

-- doc
GET /api/definitions/terms/by_name/doc@icfnhas71n/summary

-- test
GET /api/definitions/terms/by_name/mytest@u17p9803hd/summary

-- function
GET /api/definitions/terms/by_name/func@6ee6j48hk3/summary

-- constructor
GET /api/definitions/terms/by_name/Thing.This@altimqs66j@0/summary


-- Long type signature
GET /api/definitions/terms/by_name/funcWithLongType@ieskgcjjvu/summary

-- Long type signature with render width
GET /api/definitions/terms/by_name/funcWithLongType@ieskgcjjvu/summary?renderWidth=20
```
