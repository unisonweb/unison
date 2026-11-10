``` unison
KV.parse : Text -> Optional (Map Text Text)
KV.parse t =
  use Pattern capture join many or some
  use Text.patterns literal letter digit notCharIn eof
  key = capture (some (or letter digit))
  value = capture (many (notCharIn [?;]))
  pair = join [key, literal "=", value]
  rest = many (join [literal ";", pair])
  whole = join [pair, rest, eof]
  pairsOf captures = Map.fromList (List.filterMap (cases
    [k, v] -> Some (k, v)
    _ -> None) (List.chunk 2 captures))
  if t == "" then Some Map.empty
  else match Pattern.run whole t with
    Some (captures, _) -> Some (pairsOf captures)
    None -> None

test> KV.parse.tests.examples = test.verify do
  ensureEqual (Optional.map Map.toList (KV.parse "host=example.com;port=8080")) (Some [("host", "example.com"), ("port", "8080")])
  ensureEqual (Optional.map Map.toList (KV.parse "a=;b=x y z")) (Some [("a", ""), ("b", "x y z")])
  ensureEqual (Optional.map Map.size (KV.parse "")) (Some 0)
  ensureEqual (KV.parse "host") None
  ensureEqual (KV.parse "a b=1") None
  ensureEqual (KV.parse "a=1;") None
  ensureEqual (KV.parse "=1") None
```
``` ucm
scratch/main> add
scratch/main> test
```
