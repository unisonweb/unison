
```ucm:hide
.> builtins.merge
```

```unison
unique ability Abort where
  abort: b

roundTrip: Nat -> Boolean
roundTrip n = 
  checkDecode: Optional (Nat, Bytes) -> {Abort} Bytes
  checkDecode = cases
    Some (n', bs) -> if n == n' then
                        watch "pass" bs
                     else
                        !(watch "fail")
                        abort

  checkDecodes bs = cases
    [] -> bs
    dec +: rest -> checkDecodes (checkDecode (dec bs)) rest
    
  go: Nat -> {Abort} Boolean
  go n = 
    bs = ((encodeNat64be n) ++ ((encodeNat32be n) ++ ((encodeNat16be n) ++ ((encodeNat64le n) ++ ((encodeNat32le n) ++ (encodeNat16le n))))))
    
    bs' = checkDecodes bs [decodeNat64be, decodeNat32be, decodeNat16be, decodeNat64le, decodeNat32le, decodeNat16le ]
    
    
    watch "end" ((Bytes.size bs') == 0)

  handle go n with cases
    { Abort.abort -> _ } -> false
    { r } -> r



test> testRoundTrip = runs 100 '(expect roundTrip (natIn 0 1000))
```

