# Continuations across optimization settings

A continuation captured inside code the explosion rewrote is written out as the frames the unoptimized code has at that point (JIT/design.md, "Continuations across processes"), so that any process can resume it whatever its own settings. This transcript is one side of that: run with `UNISON_CONT_ROLE=send`, it captures a continuation inside each rewritten shape, after 0, 1 and 2 requests, serializes it to `UNISON_CONT_DIR/<shape>-<n>-<UNISON_CONT_LABEL>.bin` and checks its own round trip; run with `UNISON_CONT_ROLE=recv`, it reads every sender's files, resumes each continuation and checks the result against a run without serialization. `scripts/jit-continuation-matrix.sh` runs every sender against every recipient, under each optimization setting (`UNISON_OPTIMIZATIONS`), in the interpreter and under the JIT (senders not under `eager`, where serializing a continuation that spans a call fails with "black hole", a known failure noted in development.md); a line that says `FAIL` is a failure, and so is a transcript that does not finish.

Run against the `jit` testing codebase (`scripts/testing-codebase.sh jit`), which has base.

``` unison
use Nat + - * < >= >

structural ability Ask where
  ask : Nat

-- a product-return worker whose body is suspended at a request
step : Nat -> Nat ->{Ask} (Nat, Nat)
step acc i =
  x = Ask.ask
  (acc + x, i + 1)

stateLoop : Nat ->{Ask} Nat
stateLoop n =
  go acc i =
    if i > n then acc
    else match step acc i with (acc', i') -> go acc' i'
  go 0 0

-- a nested product result, taken apart in full
nested : Nat ->{Ask} ((Nat, Nat), Nat)
nested i =
  x = Ask.ask
  ((i + x, i + 1), i + 2)

nestedLoop : Nat ->{Ask} Nat
nestedLoop n =
  go acc i =
    if i > n then acc
    else match nested i with ((a, b), c) -> go (acc + a + b + c) (i + 1)
  go 0 0

-- the inner product of a result still named as a whole
nestedWhole : Nat ->{Ask} Nat
nestedWhole n =
  go acc i =
    if i > n then acc
    else match nested i with (ab, c) -> go (acc + at1 ab + c) (i + 1)
  go 0 0

-- a loop whose tuple state is passed as fields, suspended inside the worker
tupleLoop : Nat ->{Ask} Nat
tupleLoop n =
  go st = match st with
    (acc, i) ->
      if i > n then acc
      else
        x = Ask.ask
        go (acc + x, i + 1)
  go (0, 0)

-- the state taken apart again inside a let's binding, where the arm's
-- variables are the worker's parameters in the enclosing frame
tupleInner : Nat ->{Ask} Nat
tupleInner n =
  go st = match st with
    (acc, i) ->
      if i > n then acc
      else
        y = match st with
          (a, _) ->
            x = Ask.ask
            a + x
        go (y, i + 1)
  go (0, 0)

-- a three-field constructor
structural type Pos = Pos Nat Nat Nat

walk : Nat ->{Ask} Nat
walk n =
  go st = match st with
    Pos i line col ->
      if i > n then line * 100 + col
      else
        x = Ask.ask
        if Nat.mod x 2 == 0 then go (Pos (i + 1) (line + 1) 0)
        else go (Pos (i + 1) line (col + x))
  go (Pos 0 0 0)

-- a record state updated each round
recLoop : Nat ->{Ask} Nat
recLoop n =
  go : { i: Nat, line: Nat, col: Nat } ->{Ask} Nat
  go st = if st@i > n then st@line * 100 + st@col
          else
            x = Ask.ask
            if Nat.mod x 2 == 0 then go (((st@i.set (st@i + 1))@line.set (st@line + x))@col.set 0)
            else go ((st@i.set (st@i + 1))@col.set (st@col + x))
  go { i: 0, line: 0, col: 0 }

-- a sum state built in the branches of a match, each branch calling
sumLoop : Nat ->{Ask} Nat
sumLoop n =
  go st = match st with
    Left acc ->
      if acc > n then acc
      else
        x = Ask.ask
        go (if Nat.mod x 2 == 0 then Left (acc + x) else Right (acc + x))
    Right acc -> go (Left (acc + 1))
  go (Left 0)

-- a product that is both a worker's result and a worker's argument
stepPair : (Nat, Nat) ->{Ask} (Nat, Nat)
stepPair = cases
  (acc, i) ->
    x = Ask.ask
    (acc + x, i + 1)

pairLoop : Nat ->{Ask} Nat
pairLoop n =
  go st = match st with
    (acc, i) -> if i > n then acc else go (stepPair (acc, i))
  go (0, 0)

-- the state kept as a whole after the call: the constructor still built
keepState : Nat ->{Ask} Nat
keepState n =
  go acc i =
    if i > n then acc
    else
      st = (acc, i)
      r = tupleLoop (Nat.mod (at1 st + at2 st) 3)
      x = Ask.ask
      go (r + x) (i + 1)
  go 0 0

-- answers every request with x, x + 1, ...
answer : Nat -> Request {Ask} Nat -> Nat
answer x = cases
  { r } -> r
  { Ask.ask -> k } -> handle k x with answer (x + 1)

-- a handler installed inside the rewritten code, with a request to the
-- outer handler under it. Not among the shapes run: a continuation
-- captured under a handler installed inside the computation does not
-- survive serialization today, with or without the explosion
-- ("reifyValue: cannot restore value: a partial application of 1
-- arguments to a function of 1"), which is a separate problem.
innerHandled : Nat ->{Ask} Nat
innerHandled n =
  go acc i =
    if i > n then acc
    else
      inner = handle
                x = Ask.ask
                x + 1
              with answer (i * 7)
      outer = Ask.ask
      match step (acc + inner) outer with (acc', i') -> go acc' (i' + i)
  go 0 0

structural type Outcome = Done Nat | Suspended (Nat ->{Ask} Nat)

-- answers the first m requests with 1, 2, ... and stops at the next with its continuation
suspendAt : Nat -> '{Ask} Nat -> Outcome
suspendAt m c =
  h : Nat -> Request {Ask} Nat -> Outcome
  h j = cases
    { r } -> Done r
    { Ask.ask -> k } -> if j == m then Suspended k else handle k (j + 1) with h (j + 1)
  handle !c with h 0

finish : Outcome -> Nat
finish = cases
  Done r -> r
  Suspended k -> handle k 100 with answer 101

shapes : [(Text, '{Ask} Nat)]
shapes =
  [ ("state", '(stateLoop 5)),
    ("nested", '(nestedLoop 5)),
    ("nestedWhole", '(nestedWhole 5)),
    ("tuple", '(tupleLoop 5)),
    ("tupleInner", '(tupleInner 5)),
    ("walk", '(walk 5)),
    ("rec", '(recLoop 5)),
    ("sum", '(sumLoop 5)),
    ("pair", '(pairLoop 5)),
    ("keep", '(keepState 3)) ]

each : [a] -> (a ->{IO, Exception} ()) ->{IO, Exception} ()
each xs f = match xs with
  [] -> ()
  x +: rest -> f x
               each rest f

main : '{IO, Exception} ()
main = do
  dir = IO.getEnv "UNISON_CONT_DIR"
  role = IO.getEnv "UNISON_CONT_ROLE"
  label = IO.getEnv "UNISON_CONT_LABEL"
  path name m s = FilePath (dir ++ "/" ++ name ++ "-" ++ Nat.toText m ++ "-" ++ s ++ ".bin")
  resumeBytes : Bytes ->{IO, Exception} Nat
  resumeBytes bs = match ##Value.deserialize bs with
    Left err -> bug ("deserialize: " ++ err)
    Right v -> match ##Value.load v with
      Left missing -> bug ("load: missing " ++ Nat.toText (List.size missing))
      Right k -> finish (Suspended k)
  verdict r direct = (if r == direct then "OK" else "FAIL") ++ " " ++ Nat.toText r ++ " expected " ++ Nat.toText direct
  runShape : (Text, '{Ask} Nat) ->{IO, Exception} ()
  runShape = cases
    (name, c) ->
      runCase : Nat ->{IO, Exception} ()
      runCase m =
        direct = finish (suspendAt m c)
        fromSender : Text ->{IO, Exception} ()
        fromSender s = printLine (name ++ " " ++ Nat.toText m ++ " from " ++ s ++ ": " ++ verdict (resumeBytes (readFile (path name m s))) direct)
        if role == "send" then
          match suspendAt m c with
            Done _ -> printLine (name ++ " " ++ Nat.toText m ++ ": FAIL, no request to suspend at")
            Suspended k ->
              bs = ##Value.serialize (##Value.value k)
              writeFile (path name m label) bs
              printLine (name ++ " " ++ Nat.toText m ++ " sent by " ++ label ++ ", own round trip " ++ verdict (resumeBytes bs) direct)
        else each ["all", "none", "args", "returns"] fromSender
      each [0, 1, 2] runCase
  each shapes runShape
```

``` ucm
jit-tests/main> run main
```
