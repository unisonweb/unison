A continuation captured inside frames that hold the results of calls, which
the runtime may keep in more than one stack slot (the wide calling convention,
Unison.Runtime.Wide): the serialized continuation has one value per variable
whichever layout the writer used, and this case is read back by both.

``` unison
structural ability DC r where
  shift : ((a -> r) -> r) -> a

structural type Delayed r = Done r | Delay (Nat -> Delayed r)

hreset : Request {DC r} r -> r
hreset = cases
  { shift e -> k } ->
    e (x -> handle k x with hreset)
  { x } -> x

reset : '{DC r} r -> r
reset body = handle !body with hreset

seven : Nat -> Nat
seven x = x * 7

-- the frame at the shift holds two call results, a pair built from them and an
-- option, all live across the capture
body : Nat ->{DC (Delayed Nat)} Nat
body n =
  a = seven n
  b = seven (n + 1)
  p = (a, b)
  o = if n == 0 then None else Some (seven b)
  r = shift (k -> Delay (x -> k (x + a)))
  d = seven (r + b)
  match (p, o) with
    ((x, y), None) -> x + y + d
    ((x, y), Some z) -> x + y + z + d

suspWide : Nat -> Delayed Nat
suspWide n = reset '(Done (body n))

-- two captures in one computation, with a frame between them
twice : Nat -> Delayed Nat
twice n = reset '(Done (body n + body (n + 10)))

feed : Nat -> Delayed Nat -> Nat
feed m = cases
  Done r -> r
  Delay f -> feed m (f m)

finish : (Delayed Nat, Delayed Nat, Delayed Nat) -> Text
finish = cases (x, y, z) ->
  "(" ++ toText (feed 2 x) ++ ", " ++ toText (feed 3 y) ++ ", " ++ toText (feed 5 z) ++ ")"

mkTestCase = do
  trip = (suspWide 0, suspWide 4, twice 1)
  saveTestCase None "case-06" "v4" finish trip
  saveTestCase (Some 5) "case-06" "v5" finish trip
```

``` ucm
> add
> run mkTestCase
```
