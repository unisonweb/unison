# Definition Summary APIs

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
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

``` ucm :hide
scratch/main> add
scratch/main> alias.type ##Nat Nat
scratch/main> alias.term ##IO.putBytes.impl.v3 putBytesImpl
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
This branch has more than one term with the name
`builtin.ImmutableByteArray.fromBytes`. Please delete or rename
all but one of them, then try the update again.
```
