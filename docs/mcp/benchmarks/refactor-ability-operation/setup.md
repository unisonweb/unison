```` unison
ability Journal where
  entry : Text ->{Journal} ()

ability Sink where
  write : Text ->{Sink} ()

Journal.collect : '{g, Journal} a ->{g} ([Text], a)
Journal.collect thunk =
  go : [Text] -> Request {Journal} a ->{g} ([Text], a)
  go acc = cases
    { a } -> (acc, a)
    { Journal.entry text -> resume } -> handle resume () with go (acc :+ text)
  handle !thunk with go []

Journal.printing : '{g, Journal} a ->{g, IO, Exception} a
Journal.printing thunk =
  go : Request {Journal} a ->{g, IO, Exception} a
  go = cases
    { a } -> a
    { Journal.entry text -> resume } ->
      printLine text
      handle resume () with go
  handle !thunk with go

Journal.ignore : '{g, Journal} a ->{g} a
Journal.ignore thunk =
  go : Request {Journal} a ->{g} a
  go = cases
    { a } -> a
    { Journal.entry _ -> resume } -> handle resume () with go
  handle !thunk with go

Journal.toSink : '{g, Journal} a ->{g, Sink} a
Journal.toSink thunk =
  go : Request {Journal} a ->{g, Sink} a
  go = cases
    { a } -> a
    { Journal.entry text -> resume } ->
      Sink.write text
      handle resume () with go
  handle !thunk with go

Sink.collect : '{g, Sink} a ->{g} ([Text], a)
Sink.collect thunk =
  go : [Text] -> Request {Sink} a ->{g} ([Text], a)
  go acc = cases
    { a } -> (acc, a)
    { Sink.write text -> resume } -> handle resume () with go (acc :+ text)
  handle !thunk with go []

Inventory.receive : Text -> Nat ->{Journal} ()
Inventory.receive name qty = Journal.entry ("received " ++ Nat.toText qty ++ " " ++ name)

Inventory.audit : [(Text, Nat)] ->{Journal} Nat
Inventory.audit items =
  go : Nat -> [(Text, Nat)] ->{Journal} Nat
  go total = cases
    [] -> total
    (name, qty) +: rest ->
      if qty == 0 then Journal.entry (name ++ " is out of stock")
      else Journal.entry (name ++ ": " ++ Nat.toText qty)
      go (total + qty) rest
  go 0 items

test> Journal.tests.collect = test.verify do
  ensureEqual (Journal.collect do Journal.entry "a"; Journal.entry "b"; 5) (["a", "b"], 5)

test> Journal.tests.ignore = test.verify do
  ensureEqual (Journal.ignore do Journal.entry "a"; 5) 5

test> Journal.tests.toSink = test.verify do
  ensureEqual (Sink.collect do Journal.toSink do Journal.entry "a"; Journal.entry "b"; 7) (["a", "b"], 7)

test> Inventory.tests.audit = test.verify do
  ensureEqual (Journal.collect do Inventory.audit [("apple", 3), ("pear", 0)]) (["apple: 3", "pear is out of stock"], 3)
  ensureEqual (Journal.collect do Inventory.receive "fig" 2) (["received 2 fig"], ())
````

``` ucm
scratch/main> add
scratch/main> test
```
