You are judging whether the code behaves correctly in the Unison codebase you are connected to. A previous agent changed `type Reading` so that its constructors take the value before the sensor name: `Temp Float Text` and `Humidity Float Text`. Every function on readings must give what it gave before, for the same reading written in the new order. You may use any tool except `learn`. Don't change anything in the codebase (no `update`, `switch` or edits). A `unison` block of watch expressions evaluates without saving anything.

Send one `unison` block with these watch expressions:

````
> Reading.value (Temp 21.5 "t1")
> Reading.sensor (Humidity 40.0 "h2")
> Reading.describe (Temp 21.5 "t1")
> Reading.describe (Humidity 40.0 "h2")
> Reading.temps ["a", "b"] [1.0, 2.0]
> Reading.humidities [3.0]
> Reading.fromPairs [("c", 4.0)]
> Reading.average [Temp 1.0 "a", Temp 3.0 "b"]
> Reading.average []
> Alert.text (Alert "hot" (Temp 30.0 "t1"))
````

Expected, in order: `21.5`, `"h2"`, `"t1: 21.5 C"`, `"h2: 40.0 %"`, `[Temp 1.0 "a", Temp 2.0 "b"]`, `[Humidity 3.0 "h1"]`, `[Temp 4.0 "c"]`, `2.0`, `0.0`, `"hot (t1: 30.0 C)"`. If the block does not typecheck because a constructor has the old order, that is a failure.

Then `scratch/main> test`: every test passes.

It passes if every value matches and the tests pass.

Reply with a short paragraph of reasons, naming any value that differed, then a final line that is exactly PASS or FAIL.
