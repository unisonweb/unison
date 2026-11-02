A `Reading` type whose constructors carry a sensor name and a value, name first. Eight definitions match on it or build it: three matchers (`Reading.value`, `Reading.sensor`, `Reading.describe`, with a doc), a constructor used bare (`Reading.temps = List.map Temp ...`, zipped), a partial application (`Humidity "h1"`), a builder from pairs, and tests. `Sensor.normalize` and its test don't use `Reading` and must not change (everything else depends on the type, so its hash changes with it).

```` unison
type Reading = Temp Text Float | Humidity Text Float

Reading.value : Reading -> Float
Reading.value = cases
  Temp _ v -> v
  Humidity _ v -> v

Reading.sensor : Reading -> Text
Reading.sensor = cases
  Temp name _ -> name
  Humidity name _ -> name

Reading.describe : Reading -> Text
Reading.describe = cases
  Temp name v -> name ++ ": " ++ Float.toText v ++ " C"
  Humidity name v -> name ++ ": " ++ Float.toText v ++ " %"

Reading.describe.doc = {{
{{ docExample 1 (do (reading -> Reading.describe reading)) }} renders a reading with its unit. For example:

@eval{ Reading.describe (Temp "t1" 21.5) }
}}

Reading.temps : [Text] -> [Float] -> [Reading]
Reading.temps names values = List.zipWith Temp names values

Reading.humidities : [Float] -> [Reading]
Reading.humidities = List.map (Humidity "h1")

Reading.fromPairs : [(Text, Float)] -> [Reading]
Reading.fromPairs pairs = List.map (cases (name, v) -> Temp name v) pairs

Reading.average : [Reading] -> Float
Reading.average readings =
  values = List.map Reading.value readings
  if List.isEmpty values then 0.0 else List.foldLeft (+) 0.0 values / Nat.toFloat (List.size values)

Sensor.normalize : Text -> Text
Sensor.normalize name = Text.toLowercase (Text.replaceAll " " "_" name)

type Alert = Alert Text Reading

Alert.text : Alert -> Text
Alert.text = cases Alert msg r -> msg ++ " (" ++ Reading.describe r ++ ")"

test> Reading.tests.accessors = test.verify do
  ensureEqual (Reading.value (Temp "t1" 21.5)) 21.5
  ensureEqual (Reading.sensor (Humidity "h2" 40.0)) "h2"
  ensureEqual (Reading.describe (Humidity "h2" 40.0)) "h2: 40.0 %"

test> Reading.tests.builders = test.verify do
  ensureEqual (Reading.temps ["a", "b"] [1.0, 2.0]) [Temp "a" 1.0, Temp "b" 2.0]
  ensureEqual (Reading.humidities [3.0]) [Humidity "h1" 3.0]
  ensureEqual (Reading.fromPairs [("c", 4.0)]) [Temp "c" 4.0]
  ensureEqual (Reading.average [Temp "a" 1.0, Temp "b" 3.0]) 2.0

test> Alert.tests.text = test.verify do
  ensureEqual (Alert.text (Alert "hot" (Temp "t1" 30.0))) "hot (t1: 30.0 C)"

test> Sensor.tests.normalize = test.verify do
  ensureEqual (Sensor.normalize "Roof Top") "roof_top"
````

``` ucm
scratch/main> add
scratch/main> test
```
