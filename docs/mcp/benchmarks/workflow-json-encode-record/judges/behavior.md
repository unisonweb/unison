You are judging how `Person.toJson` and `Person.fromJson` behave in the Unison codebase you are connected to. A previous agent wrote them. You may use any tool except `learn`. Don't change the project's definitions; use scratch code (a `unison` block or `run`) only for your own checks. `Person` is a record with `name : Text` and `age : Nat`. You may need `transcript-io` for `run`; plain evaluation in a `unison` block (a watch line, `> expression`) needs only `transcript`.

Evaluate each of these and note the result (`fromJson` gives an `Either Failure Person`; only whether it is `Left` or `Right`, and the person in the `Right`, matter):

1. `Person.fromJson (Person.toJson (Person "Ada" 36))` is `Right (Person "Ada" 36)`. Also for `Person "" 0` and for a name with characters that JSON has to escape, `Person "say \"hi\"\n" 7`.
2. `Person.toJson (Person "Ada" 36)` is valid JSON text that contains the key `name` with the text `Ada` and the key `age` with the number `36` (for example `{"name":"Ada","age":36}`; spaces and the order of the keys may differ). Check it by decoding it with `Person.fromJson`, and check that the text contains `"age"` and `36` and does not put the age in quotes.
3. Text the agent did not produce is accepted: `Person.fromJson "{\"age\": 3, \"name\": \"Bo\"}"` (keys in the other order) and `Person.fromJson "  {\"name\" : \"Bo\" ,\n \"age\" : 3 }  "` (extra spaces and a newline) are `Right (Person "Bo" 3)`.
4. These all give a `Left` (and do not crash, hang or raise out of the function): `""`, `"not json"`, `"[1,2]"`, `"{}"`, `"{\"name\": \"Ada\"}"` (no age), `"{\"age\": 36}"` (no name), `"{\"name\": \"Ada\", \"age\": \"36\"}"` (age is text), `"{\"name\": 5, \"age\": 36}"` (name is a number), `"{\"name\": \"Ada\", \"age\": -1}"` (a negative age is not a `Nat`). A `Right` with a made-up default (an empty name, an age of `0`) for a missing or wrong field fails the check.
5. Extra fields are tolerated or rejected, either is fine: `Person.fromJson "{\"name\": \"Ada\", \"age\": 36, \"extra\": true}"` must not crash.

They pass only if checks 1 to 4 all hold.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
