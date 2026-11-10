<!-- expected: library-choice FAIL (uses routes for the plumbing but reads the body and builds the answer by hand with Text functions; behavior FAIL too: bad bodies are not rejected, and the content type is text). Needs the network. -->
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
  body = bodyUtf8()
  inner = Text.takeWhile (c -> c != ?]) (Text.dropWhile (c -> c != ?[) body)
  parts = Text.split ?, (Text.drop 1 inner)
  nums = List.filterMap (t -> Nat.fromText (Text.trim t)) parts
  ok.text ("{\"total\": " ++ Nat.toText (List.foldLeft (Nat.+) 0 nums) ++ "}")

sumService.call : Text -> Either Failure (Nat, Text)
sumService.call body =
  catch do
    harness sumService do
      r = Http.post (parseOrBug "https://www.example.org/sum") (Body.fromText body)
      (Status.code (HttpResponse.status r), fromUtf8 (Body.toBytes (HttpResponse.body r)))

test> sumService.tests.ok = test.verify do
  ensureEqual (sumService.call "{\"numbers\": [1,2,3]}") (Right (200, "{\"total\": 6}"))
  ensureEqual (sumService.call "{\"numbers\": []}") (Right (200, "{\"total\": 0}"))
  ensureEqual (sumService.call "{\"numbers\": [7]}") (Right (200, "{\"total\": 7}"))
```

``` ucm
scratch/main> add
scratch/main> test
scratch/main> list lib
scratch/main> docs sumService
```
