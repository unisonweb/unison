You are judging `Order.fromJson` in the Unison codebase you are connected to. The project started with `@unison/base` and the types `LineItem` (`sku : Text`, `quantity : Nat`, `priceCents : Nat`) and `Order` (`customer : Text`, `items : [LineItem]`, `note : Optional Text`). A previous agent wrote `Order.fromJson : Text ->{Exception} Order`, reading JSON text into an `Order` with a JSON library installed from Share. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Define this helper in a `unison` block of a transcript and evaluate each of the following with `>` watch lines (a `Left` means the function raised, which is what is wanted for bad input):

````
``` unison
judge.from : Text -> Either Failure Order
judge.from t = catch do Order.fromJson t
```
````

1. `view Order.fromJson`: its type is exactly `Text ->{Exception} Order` (`Exception` and no other ability).
2. Valid documents give exactly the record:
   - `judge.from "{\"customer\": \"Ada\", \"items\": [{\"sku\": \"A-1\", \"quantity\": 2, \"priceCents\": 1250}], \"note\": \"gift\"}"` is `Right (Order "Ada" [LineItem "A-1" 2 1250] (Some "gift"))`.
   - `judge.from "{\"customer\": \"Bo\", \"items\": []}"` is `Right (Order "Bo" [] None)` (no `note`).
   - Keys in another order and extra spaces and newlines: `judge.from "  {\"items\" : [ {\"priceCents\": 5, \"quantity\": 1, \"sku\": \"x y\"} , {\"sku\": \"B\", \"quantity\": 3, \"priceCents\": 4000000000} ],\n \"note\" : \"say \\\"hi\\\"\", \"customer\": \"Cy Young\" }  "` is `Right (Order "Cy Young" [LineItem "x y" 1 5, LineItem "B" 3 4000000000] (Some "say \"hi\""))`.
   - An unknown extra field is tolerated or rejected, either is fine, but it must not crash the evaluation: `judge.from "{\"customer\": \"Ada\", \"items\": [], \"extra\": true}"`.
3. All of these are a `Left` (none returns an `Order`, none hangs or crashes the evaluation):
   - invalid JSON: `""`, `"not json"`, `"{\"customer\": \"Ada\", \"items\": ["` (truncated);
   - JSON of another shape: `"[1, 2]"`, `"\"Ada\""`, `"{}"`;
   - a missing required field: `"{\"items\": []}"` (no customer), `"{\"customer\": \"Ada\"}"` (no items), and an item without `quantity`: `"{\"customer\": \"Ada\", \"items\": [{\"sku\": \"A\", \"priceCents\": 5}]}"`;
   - a field of the wrong type: `"{\"customer\": 7, \"items\": []}"`, `"{\"customer\": \"Ada\", \"items\": {}}"`, `"{\"customer\": \"Ada\", \"items\": [{\"sku\": \"A\", \"quantity\": \"2\", \"priceCents\": 5}]}"`, a negative or fractional `quantity` (`-1`, `1.5`), and a `note` that is a number: `"{\"customer\": \"Ada\", \"items\": [], \"note\": 5}"`.
   A `Right` that fills in a default (an empty customer, a quantity of `0`) for a missing or wrong field fails the check.
4. Evaluating the first valid document twice in separate blocks gives the same result.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
