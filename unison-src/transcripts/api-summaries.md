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
GET /api/definitions/terms/by-hash/@qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8/summary?name=nat

-- term without name uses hash
GET /api/definitions/terms/by-hash/@qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8/summary

-- doc
GET /api/definitions/terms/by-hash/@icfnhas71n8q5rm7rmpe51hh7bltsr7rb4lv7qadc4cbsifu1mhonlqj2d7836iar2ptc648q9p4u7hf40ijvld574421b6u8gpu0lo/summary?name=doc

-- test
GET /api/definitions/terms/by-hash/@u17p9803hdibisou6rlr1sjbccdossgh7vtkd03ovlvnsl2n91lq94sqhughc62tnrual2jlrfk922sebp4nm22o7m5u9j40emft8r8/summary?name=mytest

-- function
GET /api/definitions/terms/by-hash/@6ee6j48hk3eovokflkgbmpbfr3oqj4hedqn8ocg3i4i0ko8j7nls7njjirmnh4k2bg8h95seaot798uuloqk62u2ttiqoceulkbmq2o/summary?name=func

-- constructor
GET /api/definitions/terms/by-hash/@altimqs66j3dh94dpab5pg7j5adjrndq61n803j7fg0v0ohdiut6or66bu1fiongpd45s5euiuo8ru47b928aqv8osln1ikdeg05hq0@d0/summary?name=Thing.This

-- Long type signature
GET /api/definitions/terms/by-hash/@ieskgcjjvuegpecq9pbha59ttonke7pf31keeq0jlh31ijkfq00e06fdi36ae90u24pjva6ucqdbedropjgi3g3b75nu76ll5ls8ke8/summary?name=funcWithLongType

-- Long type signature with render width
GET /api/definitions/terms/by-hash/@ieskgcjjvuegpecq9pbha59ttonke7pf31keeq0jlh31ijkfq00e06fdi36ae90u24pjva6ucqdbedropjgi3g3b75nu76ll5ls8ke8/summary?renderWidth=20&name=funcWithLongType

-- Builtin Term
GET /api/definitions/terms/by-hash/@@IO.putBytes.impl.v3/summary?name=putBytesImpl
```

## Type Summary APIs

```api
-- data
GET /api/definitions/types/by-hash/@altimqs66j3dh94dpab5pg7j5adjrndq61n803j7fg0v0ohdiut6or66bu1fiongpd45s5euiuo8ru47b928aqv8osln1ikdeg05hq0/summary?name=Thing

-- data with type args
GET /api/definitions/types/by-hash/@nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg/summary?name=Maybe

-- ability
GET /api/definitions/types/by-hash/@rfi1v9429f9qluv533l2iba77aadttilrpmnhljfapfnfa6sru2nr8ibpqvib9nc4s4nb9s1as45upsfqfqe6ivqi2p82b2vd866it8/summary?name=Stream

-- builtin type
GET /api/definitions/types/by-hash/@@Nat/summary?name=Nat
```


