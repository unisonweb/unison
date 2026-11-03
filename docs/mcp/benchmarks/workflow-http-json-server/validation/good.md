<!-- Needs the network (the library install). Run on the codebase prepare.sh makes (no setup.md: the base codebase only). -->
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
  decoder = do Decoder.object.at! "numbers" (Decoder.array Decoder.nat)
  match catch do Decoder.run decoder bodyUtf8() with
    Right numbers ->
      ok.json (Json.object [("total", Json.nat (List.foldLeft (Nat.+) 0 numbers))])
    Left _ -> badRequest.text "expected a JSON object like {\"numbers\": [1, 2, 3]}"

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

test> sumService.tests.badBody = test.verify do
  ensureEqual (Either.mapRight at1 (sumService.call "not json")) (Right 400)
  ensureEqual (Either.mapRight at1 (sumService.call "{\"numbers\": \"x\"}")) (Right 400)
  ensureEqual (Either.mapRight at1 (sumService.call "{\"numbers\": [-1]}")) (Right 400)
  ensureEqual (Either.mapRight at1 (sumService.call "[1,2,3]")) (Right 400)
```

``` ucm
scratch/main> add
scratch/main> test
scratch/main> list lib
scratch/main> docs sumService
```
