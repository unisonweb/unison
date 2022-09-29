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
structural type Maybe a = Nothing | Just a

structural ability Stream s where
  send : s -> ()
```

```ucm:hide
.> add
.> alias.type ##Nat Nat
.> alias.term ##IO.putBytes.impl.v3 putBytesImpl
```

## Term Summary APIs

```api
-- term
GET /api/definitions/terms/by-hash/@qkhkl0n238/summary?name=nat

-- term without name uses hash
GET /api/definitions/terms/by-hash/@qkhkl0n238/summary

-- doc
GET /api/definitions/terms/by-hash/@icfnhas71n/summary?name=doc

-- test
GET /api/definitions/terms/by-hash/@u17p9803hd/summary?name=mytest

-- function
GET /api/definitions/terms/by-hash/@6ee6j48hk3/summary?name=func

-- constructor
GET /api/definitions/terms/by-hash/@altimqs66j@0/summary?name=Thing.This

-- Long type signature
GET /api/definitions/terms/by-hash/@ieskgcjjvu/summary?name=funcWithLongType

-- Long type signature with render width
GET /api/definitions/terms/by-hash/@ieskgcjjvu/summary?renderWidth=20&name=funcWithLongType

-- Builtin Term
GET /api/definitions/terms/by-hash/@@IO.putBytes.impl.v3/summary?name=putBytesImpl
```

## Type Summary APIs

```api
-- data
GET /api/definitions/types/by-hash/@altimqs66j/summary?name=Thing

-- data with type args
GET /api/definitions/types/by-hash/@nirp5os0q6/summary?name=Maybe

-- ability
GET /api/definitions/types/by-hash/@rfi1v9429f/summary?name=Stream

-- builtin type
GET /api/definitions/types/by-hash/@@Nat/summary?name=Nat
```


