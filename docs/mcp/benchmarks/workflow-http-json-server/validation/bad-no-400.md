<!-- expected: behavior FAIL (decodeJson raises: a bad body does not become a 400; the tests only cover good bodies). Needs the network. -->
``` ucm
scratch/main> lib.install @unison/routes/releases/7.0.2
```

``` unison
sumService.doc = {{
`sumService` is the HTTP service for `POST /sum`. The body is a JSON object like `{"numbers": [1, 2, 3]}` with a list of natural numbers; the answer is JSON like `{"total": 6}`. A body that is not valid JSON of that shape gets a 400 answer, and any other path or method falls through to a 404.
}}

sumService : '{Route, Exception} ()
sumService = do
  noCapture POST (s "sum")
  numbers = decodeJson do Decoder.object.at! "numbers" (Decoder.array Decoder.nat)
  ok.json (Json.object [("total", Json.nat (List.foldLeft (Nat.+) 0 numbers))])

sumService.call : Text -> Either Failure (Nat, Text)
sumService.call body =
  catch do
    harness sumService do
      r = Http.post (parseOrBug "https://www.example.org/sum") (Body.fromText body)
      (Status.code (HttpResponse.status r), fromUtf8 (Body.toBytes (HttpResponse.body r)))

test> sumService.tests.ok = test.verify do
  ensureEqual (sumService.call "{\"numbers\": [1,2,3]}") (Right (200, "{\"total\":6}"))
  ensureEqual (sumService.call "{\"numbers\": []}") (Right (200, "{\"total\":0}"))
  ensureEqual (sumService.call "{\"numbers\": [7]}") (Right (200, "{\"total\":7}"))
```

``` ucm
scratch/main> add
scratch/main> test
scratch/main> list lib
scratch/main> docs sumService
```
