You are judging how the service `sumService` behaves in the Unison codebase you are connected to. A previous agent built it from libraries it found on Unison Share: `POST /sum` takes a JSON body like `{"numbers": [1, 2, 3]}` (natural numbers) and answers with JSON like `{"total": 6}`; a body that is not valid JSON of that shape gets a 400 answer. You may use any tool except `learn`, and a transcript with the `transcript` tool is enough. Don't change the project's definitions; use scratch code only for your own checks, and never start a server: no `serveSimple`, `serve` or `run` of the service, and nothing that opens a port or waits for input. Don't depend on Unison Share being reachable.

First run `view sumService` and read its type. 

If its type is a `Route` computation (for example `'{Route, Exception} ()`, the usual shape with `@unison/routes`), call it through the library's test harness, which runs the service on a fake request in memory and opens no port. Use exactly this scratch block in a transcript (adjust nothing but the abilities in `judge.call`'s type if the compiler asks, and say so in your reply if you did):

````
``` unison
judge.call : Text -> Text -> Text -> Either Failure (Nat, Text)
judge.call method path body =
  catch do
    harness sumService do
      uri = parseOrBug ("https://www.example.org" ++ path)
      r = if method == "GET" then Http.get uri else Http.post uri (Body.fromText body)
      (Status.code (HttpResponse.status r), fromUtf8 (Body.toBytes (HttpResponse.body r)))

> judge.call "POST" "/sum" "{\"numbers\": [1, 2, 3]}"
> judge.call "POST" "/sum" "{\"numbers\": []}"
> judge.call "POST" "/sum" "{ \"numbers\" : [ 5 ] }"
> judge.call "POST" "/sum" "{\"numbers\": [1000000, 2000000, 3000000000]}"
> judge.call "POST" "/sum" "{\"other\": 1, \"numbers\": [4, 5]}"
> judge.call "POST" "/sum" "not json"
> judge.call "POST" "/sum" ""
> judge.call "POST" "/sum" "{}"
> judge.call "POST" "/sum" "{\"numbers\": \"x\"}"
> judge.call "POST" "/sum" "{\"numbers\": [1, \"a\"]}"
> judge.call "POST" "/sum" "[1, 2, 3]"
> judge.call "GET" "/sum" ""
> judge.call "POST" "/other" "{\"numbers\": [1]}"
```
````

If its type is something else (for example a `Handler` or a function from `HttpRequest` to `HttpResponse`, written directly against `@unison/http`, where `Handler` wraps `HttpRequest ->{g, Exception, Abort} HttpResponse` and an `Abort` means "not my route"), build the same requests with that library's request constructors and call the function with them under whatever abilities its type needs, without opening a port. If that cannot be done without a port, test the pieces the service is built from instead (the functions it calls to decode the body and to build the answer, found with `dependencies sumService`) on the same inputs, and say so in your reply.

The results must be, in order:

1. `Right (200, ...)` with a body that is JSON for `total` 6, `0`, `5`, `3003000000` and `9` for the first five calls. White space between tokens and the order of keys may differ (`{"total":6}` and `{"total": 6}` are both fine); the number must be exactly the sum and not in quotes.
2. `Right (400, ...)` for the six calls after that: `not json`, the empty body, `{}`, `{"numbers": "x"}`, `{"numbers": [1, "a"]}` and `[1, 2, 3]`. A `Left` (the failure escaping from the service as an exception, which a real server would turn into a 500) or a status of 200 or 500 fails the check. The text of the 400 body does not matter.
3. `GET /sum` and `POST /other` answer something that is not 200 (404 or 405 is expected); a `Left` also fails.

They pass only if checks 1 to 3 all hold. Also note, without failing on it, whether the 200 responses carry the header `Content-Type: application/json` (only if you can see it cheaply).

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
