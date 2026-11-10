<!-- The end state by hand: every definition that builds or matches on a `Reading`, redefined with the value first, in
     one block with the type, so that the update applies cleanly. The rewrite route (`validation/good.md` in the other
     refactoring tasks) can't be shown in a transcript, since it reads the file `rewrite` writes. -->

```` unison
type Reading = Temp Float Text | Humidity Float Text

Reading.value : Reading -> Float
Reading.value = cases
  Temp v _ -> v
  Humidity v _ -> v

Reading.sensor : Reading -> Text
Reading.sensor = cases
  Temp _ name -> name
  Humidity _ name -> name

Reading.describe : Reading -> Text
Reading.describe = cases
  Temp v name -> name ++ ": " ++ Float.toText v ++ " C"
  Humidity v name -> name ++ ": " ++ Float.toText v ++ " %"

Reading.describe.doc = {{
{{ docExample 1 (do (reading -> Reading.describe reading)) }} renders a reading with its unit. For example:

@eval{ Reading.describe (Temp 21.5 "t1") }
}}

Reading.temps : [Text] -> [Float] -> [Reading]
Reading.temps names values = List.zipWith (name v -> Temp v name) names values

Reading.humidities : [Float] -> [Reading]
Reading.humidities = List.map (v -> Humidity v "h1")

Reading.fromPairs : [(Text, Float)] -> [Reading]
Reading.fromPairs pairs = List.map (cases (name, v) -> Temp v name) pairs

Reading.average : [Reading] -> Float
Reading.average readings =
  values = List.map Reading.value readings
  if List.isEmpty values then 0.0 else List.foldLeft (+) 0.0 values / Nat.toFloat (List.size values)

type Alert = Alert Text Reading

Alert.text : Alert -> Text
Alert.text = cases Alert msg r -> msg ++ " (" ++ Reading.describe r ++ ")"

test> Reading.tests.accessors = test.verify do
  ensureEqual (Reading.value (Temp 21.5 "t1")) 21.5
  ensureEqual (Reading.sensor (Humidity 40.0 "h2")) "h2"
  ensureEqual (Reading.describe (Humidity 40.0 "h2")) "h2: 40.0 %"

test> Reading.tests.builders = test.verify do
  ensureEqual (Reading.temps ["a", "b"] [1.0, 2.0]) [Temp 1.0 "a", Temp 2.0 "b"]
  ensureEqual (Reading.humidities [3.0]) [Humidity 3.0 "h1"]
  ensureEqual (Reading.fromPairs [("c", 4.0)]) [Temp 4.0 "c"]
  ensureEqual (Reading.average [Temp 1.0 "a", Temp 3.0 "b"]) 2.0

test> Alert.tests.text = test.verify do
  ensureEqual (Alert.text (Alert "hot" (Temp 30.0 "t1"))) "hot (t1: 30.0 C)"
````

``` ucm
scratch/main> update
scratch/main> test
scratch/main> view Reading
scratch/main> docs Reading.describe
scratch/main> names Sensor.normalize
scratch/main> names Sensor.tests.normalize
scratch/main> find
scratch/main> branches
scratch/main> todo
```

``` unison
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
```
