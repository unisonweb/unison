``` unison
Port.parse : Text ->{Exception} Nat
Port.parse t = match Nat.fromText t with
  None -> Exception.raiseGeneric "not a number" t
  Some n ->
    if n == 0 || n > 65535 then Exception.raiseGeneric "port out of range 1 to 65535" n
    else n

Port.parseOrNone : Text -> Optional Nat
Port.parseOrNone t = Either.toOptional (catch do Port.parse t)

test> Port.tests.parse = test.verify do
  ensureEqual (catch do Port.parse "8080") (Right 8080)
  ensureEqual (catch do Port.parse "1") (Right 1)
  ensureEqual (catch do Port.parse "65535") (Right 65535)
  ensure (Either.isLeft (catch do Port.parse "0"))
  ensure (Either.isLeft (catch do Port.parse "65536"))
  ensure (Either.isLeft (catch do Port.parse "http"))
  ensure (Either.isLeft (catch do Port.parse "80abc"))

test> Port.tests.parseOrNone = test.verify do
  ensureEqual (Port.parseOrNone "443") (Some 443)
  ensureEqual (Port.parseOrNone "http") None
  ensureEqual (Port.parseOrNone "0") None
  ensureEqual (Port.parseOrNone "-5") None
```
``` ucm
scratch/main> add
scratch/main> test
```
