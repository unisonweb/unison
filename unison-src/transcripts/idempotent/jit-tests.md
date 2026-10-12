# JIT tests

Programs that exercise the parts of the runtime the JIT compiles. Each one is small and has a
known answer. The transcript is run with the JIT off and on, and the output must be the same.
See `docs/jit/implementation-plan.md`.

This transcript uses only builtins, so it runs in an empty codebase.

``` ucm :hide
scratch/main> builtins.mergeio
```

A few definitions the base library would normally provide.

``` unison :hide
(Nat.-) : Nat -> Nat -> Nat
(Nat.-) a b = Nat.drop a b

(Nat.==) : Nat -> Nat -> Boolean
(Nat.==) a b = Nat.eq a b

(Nat.<) : Nat -> Nat -> Boolean
(Nat.<) a b = Nat.lt a b

(Nat.>) : Nat -> Nat -> Boolean
(Nat.>) a b = Nat.gt a b

(Nat.<=) : Nat -> Nat -> Boolean
(Nat.<=) a b = Nat.lteq a b

(Nat.>=) : Nat -> Nat -> Boolean
(Nat.>=) a b = Nat.gteq a b
```

``` ucm :hide
scratch/main> update
```

## Arithmetic and loops

Self tail calls, arithmetic and comparisons on unboxed values.

``` unison
use Nat + - * / == < > <= >=

sumTo : Nat -> Nat
sumTo n =
  go acc i = if i > n then acc else go (acc + i) (i + 1)
  go 0 0

countDown : Nat -> Nat
countDown n = if n == 0 then 0 else countDown (n - 1)

collatz : Nat -> Nat
collatz n =
  go steps k =
    if k == 1 then steps
    else if Nat.mod k 2 == 0 then go (steps + 1) (k / 2)
    else go (steps + 1) (3 * k + 1)
  go 0 n

intLoop : Int -> Int
intLoop n =
  go acc i = if Int.gt i n then acc else go (acc Int.- i Int.* i) (i Int.+ +1)
  go +0 -10

floatLoop : Nat -> Float
floatLoop n =
  go acc i = if i == n then acc else go (acc Float.+ (Nat.toFloat i Float./ 2.0)) (i + 1)
  go 0.0 0

-- mutual recursion through tail calls to known functions
isEven : Nat -> Boolean
isEven n = if n == 0 then true else isOdd (n - 1)

isOdd : Nat -> Boolean
isOdd n = if n == 0 then false else isEven (n - 1)

> sumTo 1000000
> countDown 1000000
> collatz 27
> intLoop +10
> floatLoop 1000
> (isEven 100000, isOdd 100000)
> (Nat.shiftLeft 1 40, Nat.and 255 1023, Nat.xor 5 3, Nat.pow 3 20, 0 - 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + collatz   : Nat -> Nat
  + countDown : Nat -> Nat
  + floatLoop : Nat -> Float
  + intLoop   : Int -> Int
  + isEven    : Nat -> Boolean
  + isOdd     : Nat -> Boolean
  + sumTo     : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    36 | > sumTo 1000000
           ⧩
           500000500000

    37 | > countDown 1000000
           ⧩
           0

    38 | > collatz 27
           ⧩
           111

    39 | > intLoop +10
           ⧩
           -770

    40 | > floatLoop 1000
           ⧩
           249750.0

    41 | > (isEven 100000, isOdd 100000)
           ⧩
           (true, false)

    42 | > (Nat.shiftLeft 1 40, Nat.and 255 1023, Nat.xor 5 3, Nat.pow 3 20, 0 - 1)
           ⧩
           (1099511627776, 255, 6, 3486784401, 0)
```

## Non-tail recursion

``` unison
use Nat + - * / == < > <= >=

fib : Nat -> Nat
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

-- deep recursion: must not overflow the C stack
depth : Nat -> Nat
depth n = if n == 0 then 0 else 1 + depth (n - 1)

ackermann : Nat -> Nat -> Nat
ackermann m n =
  if m == 0 then n + 1
  else if n == 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

-- mutual non-tail recursion across two definitions
evenDepth : Nat -> Nat
evenDepth n = if n == 0 then 0 else 1 + oddDepth (n - 1)

oddDepth : Nat -> Nat
oddDepth n = if n == 0 then 1 else 1 + evenDepth (n - 1)

-- three calls deep; only the innermost hands over to the interpreter
inner : Nat -> Nat
inner n = Text.size (Nat.toText n) + n

middle : Nat -> Nat
middle n = inner n + 1

outer : Nat -> Nat
outer n = middle n + 1

-- a call inside a `let` binding's own `let`: its frame is not the function's
letInBinding : Nat -> Nat
letInBinding n =
  x =
    y = inner n
    y + 1
  x * 2

> fib 20
> depth 1000000
> ackermann 2 10
> evenDepth 100001
> outer 41
> letInBinding 41
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ackermann    : Nat -> Nat -> Nat
  + depth        : Nat -> Nat
  + evenDepth    : Nat -> Nat
  + fib          : Nat -> Nat
  + inner        : Nat -> Nat
  + letInBinding : Nat -> Nat
  + middle       : Nat -> Nat
  + oddDepth     : Nat -> Nat
  + outer        : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    41 | > fib 20
           ⧩
           6765

    42 | > depth 1000000
           ⧩
           1000000

    43 | > ackermann 2 10
           ⧩
           23

    44 | > evenDepth 100001
           ⧩
           100002

    45 | > outer 41
           ⧩
           45

    46 | > letInBinding 41
           ⧩
           88
```

## Data: allocation and pattern matching

``` unison
use Nat + - * / == < > <= >=

structural type Cons a = Nil | Cons a (Cons a)

Cons.range : Nat -> Nat -> Cons Nat
Cons.range lo hi =
  go acc i = if i == lo then Cons.Cons i acc else go (Cons.Cons i acc) (i - 1)
  if hi <= lo then Cons.Nil else go Cons.Nil (hi - 1)

Cons.map : (a ->{g} b) -> Cons a ->{g} Cons b
Cons.map f = cases
  Cons.Nil -> Cons.Nil
  Cons.Cons h t -> Cons.Cons (f h) (Cons.map f t)

Cons.foldLeft : (b ->{g} a ->{g} b) -> b -> Cons a ->{g} b
Cons.foldLeft f z = cases
  Cons.Nil -> z
  Cons.Cons h t -> Cons.foldLeft f (f z h) t

Cons.size : Cons a -> Nat
Cons.size c = Cons.foldLeft (n _ -> n + 1) 0 c

structural type Tree = Leaf | Node Tree Nat Nat Tree

Tree.insert : Nat -> Nat -> Tree -> Tree
Tree.insert k v = cases
  Tree.Leaf -> Tree.Node Tree.Leaf k v Tree.Leaf
  Tree.Node l k2 v2 r ->
    if k < k2 then Tree.Node (Tree.insert k v l) k2 v2 r
    else if k > k2 then Tree.Node l k2 v2 (Tree.insert k v r)
    else Tree.Node l k v r

Tree.lookup : Nat -> Tree -> Optional Nat
Tree.lookup k = cases
  Tree.Leaf -> None
  Tree.Node l k2 v r ->
    if k < k2 then Tree.lookup k l
    else if k > k2 then Tree.lookup k r
    else Some v

Tree.size : Tree -> Nat
Tree.size = cases
  Tree.Leaf -> 0
  Tree.Node l _ _ r -> Tree.size l + 1 + Tree.size r

-- inserts keys in a scrambled order, so the tree is reasonably balanced
Tree.build : Nat -> Tree
Tree.build n =
  go t i =
    if i == n then t
    else go (Tree.insert (Nat.mod (i * 7919) 10007) i t) (i + 1)
  go Tree.Leaf 0

structural type Shape = Circle Nat | Rect Nat Nat | Tri Nat Nat Nat | Dot

area : Shape -> Nat
area = cases
  Shape.Circle r -> 3 * r * r
  Shape.Rect w h -> w * h
  Shape.Tri a b c -> a + b + c
  Shape.Dot -> 0

shapes : Nat -> Nat
shapes n =
  go acc i =
    if Nat.eq i n then acc
    else
      s = match Nat.mod i 4 with
        0 -> Shape.Circle i
        1 -> Shape.Rect i 2
        2 -> Shape.Tri i i i
        _ -> Shape.Dot
      go (acc + area s) (i + 1)
  go 0 0

-- sums a fresh list of n cells, k times: allocates far more than the
-- nursery, so it needs many collections while native code holds the loop
Cons.sum : Cons Nat -> Nat
Cons.sum = cases
  Cons.Nil -> 0
  Cons.Cons h t -> h + Cons.sum t

churn : Nat -> Nat -> Nat
churn k n =
  go i acc = if i == 0 then acc else go (i - 1) (acc + Cons.sum (Cons.range 0 n))
  go k 0

> Cons.size (Cons.range 0 100000)
> Cons.foldLeft (+) 0 (Cons.map (x -> x * 2) (Cons.range 0 1000))
> Tree.size (Tree.build 5000)
> (Tree.lookup 7919 (Tree.build 5000), Tree.lookup 10008 (Tree.build 5000))
> shapes 10000
> (1, "two", 3.0, ?4, +5, (6, 7))
> churn 40 50000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Cons a
  + structural type Shape
  + structural type Tree

  + area          : Shape -> Nat
  + churn         : Nat -> Nat -> Nat
  + Cons.foldLeft : (b ->{g} a ->{g} b) -> b -> Cons a ->{g} b
  + Cons.map      : (a ->{g} b) -> Cons a ->{g} Cons b
  + Cons.range    : Nat -> Nat -> Cons Nat
  + Cons.size     : Cons a -> Nat
  + Cons.sum      : Cons Nat -> Nat
  + shapes        : Nat -> Nat
  + Tree.build    : Nat -> Tree
  + Tree.insert   : Nat -> Nat -> Tree -> Tree
  + Tree.lookup   : Nat -> Tree -> Optional Nat
  + Tree.size     : Tree -> Nat

  Run `update` to apply these changes to your codebase.

    88 | > Cons.size (Cons.range 0 100000)
           ⧩
           100000

    89 | > Cons.foldLeft (+) 0 (Cons.map (x -> x * 2) (Cons.range 0 1000))
           ⧩
           999000

    90 | > Tree.size (Tree.build 5000)
           ⧩
           5000

    91 | > (Tree.lookup 7919 (Tree.build 5000), Tree.lookup 10008 (Tree.build 5000))
           ⧩
           (Some 1, None)

    92 | > shapes 10000
           ⧩
           249912515000

    93 | > (1, "two", 3.0, ?4, +5, (6, 7))
           ⧩
           (1, "two", 3.0, ?4, +5, (6, 7))

    94 | > churn 40 50000
           ⧩
           49999000000
```

## Calls to function values

``` unison
use Nat + - * / == < > <= >=

applyN : Nat -> (a ->{g} a) -> a ->{g} a
applyN n f a = if n == 0 then a else applyN (n - 1) f (f a)

compose : (b ->{g} c) -> (a ->{g} b) -> a ->{g} c
compose f g a = f (g a)

add3 : Nat -> Nat -> Nat -> Nat
add3 a b c = a + b + c

-- a closure that captures a variable
adder : Nat -> (Nat -> Nat)
adder k = x -> x + k

-- too few arguments, then the rest
partial : Nat
partial =
  f = add3 1
  g = f 2
  g 3

-- too many arguments: `pick` takes one argument and returns a function
pick : Boolean -> (Nat -> Nat -> Nat)
pick b = if b then (Nat.+) else (Nat.*)

> applyN 100000 (x -> x + 3) 0
> applyN 1000 (adder 7) 1
> applyN 10 (compose (adder 1) (x -> x * 2)) 1
> partial
> (pick true 6 7, pick false 6 7)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + add3    : Nat -> Nat -> Nat -> Nat
  + adder   : Nat -> Nat -> Nat
  + applyN  : Nat -> (a ->{g} a) -> a ->{g} a
  + compose : (b ->{g} c) -> (a ->{g} b) -> a ->{g} c
  + partial : Nat
  + pick    : Boolean -> Nat -> Nat -> Nat

  Run `update` to apply these changes to your codebase.

    27 | > applyN 100000 (x -> x + 3) 0
           ⧩
           300000

    28 | > applyN 1000 (adder 7) 1
           ⧩
           7001

    29 | > applyN 10 (compose (adder 1) (x -> x * 2)) 1
           ⧩
           2047

    30 | > partial
           ⧩
           6

    31 | > (pick true 6 7, pick false 6 7)
           ⧩
           (13, 42)
```

## Builtins implemented in Haskell

Lists, text and bytes are handled by the interpreter, in the middle of compiled code.

``` unison
use Nat + - * / == < > <= >=

listSum : [Nat] -> Nat
listSum = cases
  [] -> 0
  h +: t -> h + listSum t

listRange : Nat -> [Nat]
listRange n =
  go acc i = if i == n then acc else go (acc :+ i) (i + 1)
  go [] 0

listRev : [a] -> [a]
listRev xs =
  go acc = cases
    [] -> acc
    init :+ last -> go (acc :+ last) init
  go [] xs

lastTwo : [Nat] -> Nat
lastTwo = cases
  _ ++ [a, b] -> a + b
  _ -> 0

textLoop : Nat -> Text
textLoop n =
  go acc i = if i == n then acc else go (acc Text.++ Nat.toText i) (i + 1)
  go "" 0

> listSum (listRange 1000)
> listRev (listRange 10)
> lastTwo (listRange 100)
> List.size (listRange 100000)
> textLoop 20
> Text.size (textLoop 1000)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lastTwo   : [Nat] -> Nat
  + listRange : Nat -> [Nat]
  + listRev   : [a] -> [a]
  + listSum   : [Nat] -> Nat
  + textLoop  : Nat -> Text

  Run `update` to apply these changes to your codebase.

    30 | > listSum (listRange 1000)
           ⧩
           499500

    31 | > listRev (listRange 10)
           ⧩
           [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

    32 | > lastTwo (listRange 100)
           ⧩
           197

    33 | > List.size (listRange 100000)
           ⧩
           100000

    34 | > textLoop 20
           ⧩
           "012345678910111213141516171819"

    35 | > Text.size (textLoop 1000)
           ⧩
           2890
```

## Abilities and continuations

These run in the interpreter. Compiled code has to hand over to it and take control back
correctly.

``` unison
use Nat + - * / == < > <= >=

structural ability Counter where
  next : Nat

structural ability Abort where
  abort : a

structural ability Choose where
  choose : Boolean

runCounter : Nat -> '{Counter, g} a ->{g} a
runCounter start thunk =
  h : Nat -> Request {Counter} a -> a
  h n = cases
    { a } -> a
    { Counter.next -> k } -> handle k n with h (n + 1)
  handle !thunk with h start

runAbort : '{Abort, g} a ->{g} Optional a
runAbort thunk =
  h : Request {Abort} a -> Optional a
  h = cases
    { a } -> Some a
    { Abort.abort -> _ } -> None
  handle !thunk with h

-- resumes each continuation twice
runChoose : '{Choose, g} a ->{g} [a]
runChoose thunk =
  h : Request {Choose} a -> [a]
  h = cases
    { a } -> [a]
    { Choose.choose -> k } -> (handle k true with h) List.++ (handle k false with h)
  handle !thunk with h

-- an ability operation below several frames of ordinary recursion
sumNext : Nat ->{Counter} Nat
sumNext n = if n == 0 then 0 else Counter.next + sumNext (n - 1)

findFirst : Nat -> Nat ->{Abort} Nat
findFirst target i =
  if i > 1000 then Abort.abort
  else if i * i == target then i
  else findFirst target (i + 1)

bits : Nat ->{Choose} Nat
bits n =
  if n == 0 then 0
  else
    b = if Choose.choose then 1 else 0
    b + 2 * bits (n - 1)

-- a non-tail call whose callee performs an operation: the caller's frame is
-- captured by the handler and resumed twice
pick : Nat ->{Choose} Nat
pick n = if Choose.choose then n else n + 1

addPicked : Nat ->{Choose} Nat
addPicked n =
  x = pick n
  x + 100

-- inline bindings (an `if` bound by a `let`), one of which performs an
-- operation from inside the binding
picky : Nat ->{Choose} Nat
picky n =
  x = if n == 0 then 1 else n
  y = if x > 10 then Nat.drop x 10 else x + 100
  z = if Choose.choose then y else x
  x + y + z

> runCounter 0 '(sumNext 1000)
> (runAbort '(findFirst 144 0), runAbort '(findFirst 145 0))
> runChoose '(bits 3)
> runCounter 10 '(runChoose '(Counter.next + bits 2))
> runChoose '(addPicked 5)
> (runChoose '(picky 5), runChoose '(picky 50))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural ability Abort
  + structural ability Choose
  + structural ability Counter

  + addPicked  : Nat ->{Choose} Nat
  + bits       : Nat ->{Choose} Nat
  + findFirst  : Nat -> Nat ->{Abort} Nat
  + pick       : Nat ->{Choose} Nat
  + picky      : Nat ->{Choose} Nat
  + runAbort   : '{g, Abort} a ->{g} Optional a
  + runChoose  : '{g, Choose} a ->{g} [a]
  + runCounter : Nat -> '{g, Counter} a ->{g} a
  + sumNext    : Nat ->{Counter} Nat

  Run `update` to apply these changes to your codebase.

    73 | > runCounter 0 '(sumNext 1000)
           ⧩
           499500

    74 | > (runAbort '(findFirst 144 0), runAbort '(findFirst 145 0))
           ⧩
           (Some 12, None)

    75 | > runChoose '(bits 3)
           ⧩
           [7, 3, 5, 1, 6, 2, 4, 0]

    76 | > runCounter 10 '(runChoose '(Counter.next + bits 2))
           ⧩
           [13, 11, 12, 10]

    77 | > runChoose '(addPicked 5)
           ⧩
           [105, 106]

    78 | > (runChoose '(picky 5), runChoose '(picky 50))
           ⧩
           ([215, 115], [130, 140])
```

An affine handler (one that resumes the continuation at most once, in tail position) runs
natively: the request finds the handler in the environment and calls it like any function,
and the handler's case returns the value to the requester ("Ability requests" in internals.md).
Requests with fields, in a let and in tail position; the same ability handled twice; a handler
whose case performs an outer ability's operation, and one whose case raises an exception; a
handler installed per iteration.

``` unison
use Nat + - * / == < > <= >=

structural ability Counter where
  next : Nat

structural ability Store a where
  get : a
  put : a -> ()

runCounter : Nat -> '{Counter, g} a ->{g} a
runCounter start thunk =
  h : Nat -> Request {Counter} a -> a
  h n = cases
    { a } -> a
    { Counter.next -> k } -> handle k n with h (n + 1)
  handle !thunk with h start

runStore : s -> '{Store s, g} a ->{g} a
runStore s thunk =
  h : s -> Request {Store s} a -> a
  h s = cases
    { a } -> a
    { Store.get -> k } -> handle k s with h s
    { Store.put s' -> k } -> handle k () with h s'
  handle !thunk with h s

-- a request in a let and one in tail position
loopStore : Nat ->{Store Nat} Nat
loopStore n = if n == 0 then Store.get else
  Store.put (Store.get + 1)
  loopStore (n - 1)

-- the same ability handled twice: the inner handler shadows the outer
nested : '{Store Nat} Nat
nested _ =
  Store.put 5
  inner = runStore 2 '(Store.get + Store.get)
  inner + Store.get

-- a handler whose case performs an operation of an outer ability
countingStore : s -> '{Store s, Counter, g} a ->{Counter, g} a
countingStore s thunk =
  h : s -> Request {Store s} a ->{Counter} a
  h s = cases
    { a } -> a
    { Store.get -> k } -> handle k s with h s
    { Store.put s' -> k } ->
      _ = Counter.next
      handle k () with h s'
  handle !thunk with h s

-- a handler whose case raises an exception
boundedStore : Nat -> '{Store Nat, g} a ->{Exception, g} a
boundedStore s thunk =
  h : Nat -> Request {Store Nat} a ->{Exception} a
  h s = cases
    { a } -> a
    { Store.get -> k } -> handle k s with h s
    { Store.put s' -> k } ->
      if s' > 100 then Exception.raise (Failure (typeLink Store) "too big" (Any s'))
      else handle k () with h s'
  handle !thunk with h s

catchE : '{Exception} a -> Either Text a
catchE c = handle c() with cases
  { a } -> Right a
  { Exception.raise f -> _ } -> match f with Failure _ msg _ -> Left msg

counted : '{Counter} (Nat, Nat)
counted _ =
  r = countingStore 0 '(loopStore 10)
  (r, Counter.next)

-- a handler installed per iteration
handleLoop : Nat -> Nat -> Nat
handleLoop acc n = if n == 0 then acc else
  r = runStore 1 '(Store.get + Store.get)
  handleLoop (acc + r) (n - 1)

unique ability Ask where
  ask : Nat

-- the outermost handler answers with a constant
constAsk : Nat -> '{Ask, g} a ->{g} a
constAsk v thunk =
  h : Request {Ask} a -> a
  h = cases
    { a } -> a
    { Ask.ask -> k } -> handle k v with h
  handle !thunk with h

-- a handler whose case asks the handler outside it and adds one: nested
-- n deep, n environment switches are in force at the innermost request
relay : '{Ask, g} a ->{Ask, g} a
relay thunk =
  h : Request {Ask} a ->{Ask} a
  h = cases
    { a } -> a
    { Ask.ask -> k } ->
      v = Ask.ask + 1
      handle k v with h
  handle !thunk with h

nestAsk : Nat -> '{Ask, g} a ->{Ask, g} a
nestAsk n thunk = if n == 0 then !thunk else relay '(nestAsk (n - 1) thunk)

sumAsk : Nat -> Nat ->{Ask} Nat
sumAsk acc n = if n == 0 then acc else sumAsk (acc + Ask.ask) (n - 1)

structural ability Choose where
  choose : Boolean

runChoose : '{Choose, g} a ->{g} [a]
runChoose thunk =
  h : Request {Choose} a -> [a]
  h = cases
    { a } -> [a]
    { Choose.choose -> k } -> (handle k true with h) List.++ (handle k false with h)
  handle !thunk with h

-- a case whose switch is in force when something exits to the
-- interpreter: a request to a non-affine handler (Choose resumes twice)
chooseAsk : '{Ask, g} a ->{Ask, Choose, g} a
chooseAsk thunk =
  h : Request {Ask} a ->{Ask, Choose} a
  h = cases
    { a } -> a
    { Ask.ask -> k } ->
      v = if Choose.choose then Ask.ask else Ask.ask + 10
      handle k v with h
  handle !thunk with h

> constAsk 5 '(nestAsk 3 '(Ask.ask))
> constAsk 5 '(nestAsk 40 '(sumAsk 0 100))
> constAsk 0 '(runChoose '(chooseAsk '(Ask.ask + Ask.ask)))
> runStore 0 '(loopStore 1000)
> runStore 1 nested
> runCounter 100 counted
> (catchE '(boundedStore 0 '(loopStore 50)), catchE '(boundedStore 0 '(loopStore 200)))
> handleLoop 0 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ability Ask
  + structural ability Choose
  + structural ability Counter
  + structural ability Store a

  + boundedStore  : Nat -> '{g, Store Nat} a ->{g, Exception} a
  + catchE        : '{Exception} a -> Either Text a
  + chooseAsk     : '{g, Ask} a ->{g, Choose, Ask} a
  + constAsk      : Nat -> '{g, Ask} a ->{g} a
  + counted       : '{Counter} (Nat, Nat)
  + countingStore : s
                    -> '{g, Counter, Store s} a
                    ->{g, Counter} a
  + handleLoop    : Nat -> Nat -> Nat
  + loopStore     : Nat ->{Store Nat} Nat
  + nestAsk       : Nat -> '{g, Ask} a ->{g, Ask} a
  + nested        : '{Store Nat} Nat
  + relay         : '{g, Ask} a ->{g, Ask} a
  + runChoose     : '{g, Choose} a ->{g} [a]
  + runCounter    : Nat -> '{g, Counter} a ->{g} a
  + runStore      : s -> '{g, Store s} a ->{g} a
  + sumAsk        : Nat -> Nat ->{Ask} Nat

  Run `update` to apply these changes to your codebase.

    133 | > constAsk 5 '(nestAsk 3 '(Ask.ask))
            ⧩
            8

    134 | > constAsk 5 '(nestAsk 40 '(sumAsk 0 100))
            ⧩
            4500

    135 | > constAsk 0 '(runChoose '(chooseAsk '(Ask.ask + Ask.ask)))
            ⧩
            [0, 10, 10, 20]

    136 | > runStore 0 '(loopStore 1000)
            ⧩
            1000

    137 | > runStore 1 nested
            ⧩
            9

    138 | > runCounter 100 counted
            ⧩
            (10, 110)

    139 | > (catchE '(boundedStore 0 '(loopStore 50)), catchE '(boundedStore 0 '(loopStore 200)))
            ⧩
            (Right 50, Left "too big")

    140 | > handleLoop 0 100
            ⧩
            200
```

## Preemption

A thread stuck in a loop that never allocates must still be killable. Compiled code polls at
every function entry for this.

``` unison
use Nat + - * / == < > <= >=

spin : Nat -> Nat
spin n = spin (n + 1)

killTest : '{IO} Text
killTest = do
  t = IO.forkComp '(spin 0)
  _ = IO.delay.impl 200000
  match IO.kill.impl t with
    Right _ -> "killed"
    Left _ -> "could not kill"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + killTest : '{IO} Text
  + spin     : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run killTest

  "killed"
```

## Mutable state and errors

``` unison
use Nat + - * / == < > <= >=

refLoop : Nat ->{IO} Nat
refLoop n =
  r = IO.ref 0
  go i =
    if i == n then Ref.read r
    else
      Ref.write r (Ref.read r + i)
      go (i + 1)
  go 0

safeDiv : Nat -> Nat -> Either Text Nat
safeDiv a b = if b == 0 then Left "divide by zero" else Right (a / b)

refTest : '{IO} Nat
refTest = do refLoop 10000

> (safeDiv 10 2, safeDiv 1 0)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + refLoop : Nat ->{IO} Nat
  + refTest : '{IO} Nat
  + safeDiv : Nat -> Nat -> Either Text Nat

  Run `update` to apply these changes to your codebase.

    19 | > (safeDiv 10 2, safeDiv 1 0)
           ⧩
           (Right 5, Left "divide by zero")
```

``` ucm
scratch/main> run refTest

  49995000
```

## Call-outs

Instructions native code has no version of are run by the interpreter, which then re-enters
native code after them. A foreign call that fails goes to the current `Exception` handler
instead, and a call-out inside an inline `let` binding re-enters in the middle of that binding.
A `let` inside such a binding gets its own re-entry point too.

``` unison
use Nat + - * / == < > <= >=

catchFailure : '{IO, Exception} a ->{IO} Either Text a
catchFailure c = handle c() with cases
  { a } -> Right a
  { Exception.raise f -> _ } -> match f with Failure _ msg _ -> Left msg

readPlus : MutableArray {IO} Nat -> Nat ->{IO, Exception} Nat
readPlus arr i = MutableArray.read arr i + 1

catching : '{IO} (Either Text Nat, Either Text Nat)
catching = do
  arr = IO.arrayOf 7 3
  (catchFailure '(readPlus arr 1), catchFailure '(readPlus arr 5))

callOutInBinding : Nat ->{IO} Nat
callOutInBinding n =
  r = IO.ref n
  x = if n == 0 then 1 else Ref.read r + 1
  y = if x > 100 then Ref.read r else x * 2
  y + 1

callOutTest : '{IO} (Nat, Nat, Nat)
callOutTest = do (callOutInBinding 0, callOutInBinding 20, callOutInBinding 200)

sizePlus : Nat -> Nat
sizePlus n = Text.size (Nat.toText n) + n

letInIf : Nat -> Nat
letInIf n =
  x = if n > 0 then
        y = sizePlus n
        y + 1
      else 0
  x * 2

> (letInIf 0, letInIf 41)

fill : MutableArray {IO} Nat -> Nat ->{IO, Exception} ()
fill arr i =
  if Nat.eq i (MutableArray.size arr) then ()
  else
    MutableArray.write arr i (i * i)
    fill arr (i + 1)

sumArr : MutableArray {IO} Nat -> Nat -> Nat ->{IO, Exception} Nat
sumArr arr i acc =
  if Nat.eq i (MutableArray.size arr) then acc
  else sumArr arr (i + 1) (acc + MutableArray.read arr i)

arrayTest : '{IO} (Either Text Nat, Either Text Nat, Either Text ())
arrayTest = do
  arr = IO.arrayOf 0 1000
  (catchFailure do
     fill arr 0
     sumArr arr 0 0,
   catchFailure '(MutableArray.read arr 1000),
   catchFailure '(MutableArray.write arr 1000 7))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + arrayTest        : '{IO} ( Either Text Nat,
                         Either Text Nat,
                         Either Text ())
  + callOutInBinding : Nat ->{IO} Nat
  + callOutTest      : '{IO} (Nat, Nat, Nat)
  + catchFailure     : '{IO, Exception} a ->{IO} Either Text a
  + catching         : '{IO} (Either Text Nat, Either Text Nat)
  + fill             : MutableArray {IO} Nat
                       -> Nat
                       ->{IO, Exception} ()
  + letInIf          : Nat -> Nat
  + readPlus         : MutableArray {IO} Nat
                       -> Nat
                       ->{IO, Exception} Nat
  + sizePlus         : Nat -> Nat
  + sumArr           : MutableArray {IO} Nat
                       -> Nat
                       -> Nat
                       ->{IO, Exception} Nat

  Run `update` to apply these changes to your codebase.

    37 | > (letInIf 0, letInIf 41)
           ⧩
           (0, 88)
```

``` ucm
scratch/main> run catching

  (Right 8, Left "MutableArray.read: array index out of bounds")

scratch/main> run callOutTest

  (3, 43, 201)

scratch/main> run arrayTest

  ( Right 332833500
  , Left "MutableArray.read: array index out of bounds"
  , Left "MutableArray.write: array index out of bounds"
  )
```

## Function values

A call to a function value is native when the value is a closure with compiled code and the
call is exactly saturated. Over-application (`mk 1 2`, where `mk 1` returns a function) and
under-application (`adder 5`, which builds a closure) are left to the interpreter.

``` unison
use Nat + - * / < > <= >=

applyN : Nat -> (a -> a) -> a -> a
applyN n f a = if Nat.eq n 0 then a else applyN (Nat.drop n 1) f (f a)

twice : (Nat -> Nat) -> Nat -> Nat
twice f x =
  y = f x
  f y

adder : Nat -> Nat -> Nat
adder k x = x + k

mk : Nat -> (Nat -> Nat)
mk k = adder (k * 10)

compose2 : (Nat -> Nat) -> (Nat -> Nat) -> Nat -> Nat
compose2 f g x = f (g x)

sumWith : (Nat -> Nat) -> Nat -> Nat
sumWith f n =
  go acc i = if Nat.eq i n then acc else go (acc + f i) (i + 1)
  go 0 0

> applyN 10000 (x -> x + 3) 0
> twice (adder 5) 10
> applyN 3 (adder 7) 1
> compose2 (adder 1) (adder 2) 3
> applyN 5 (twice (adder 1)) 0
> mk 1 2
> applyN 4 (mk 2) 0
> sumWith (x -> x * x) 100
> sumWith (compose2 (mk 1) (adder 1)) 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + adder    : Nat -> Nat -> Nat
  + applyN   : Nat -> (a ->{g} a) -> a ->{g} a
  + compose2 : (Nat ->{g1} Nat)
               -> (Nat ->{g} Nat)
               -> Nat
               ->{g, g1} Nat
  + mk       : Nat -> Nat -> Nat
  + sumWith  : (Nat ->{g} Nat) -> Nat ->{g} Nat
  + twice    : (Nat ->{g} Nat) -> Nat ->{g} Nat

  Run `update` to apply these changes to your codebase.

    25 | > applyN 10000 (x -> x + 3) 0
           ⧩
           30000

    26 | > twice (adder 5) 10
           ⧩
           20

    27 | > applyN 3 (adder 7) 1
           ⧩
           22

    28 | > compose2 (adder 1) (adder 2) 3
           ⧩
           6

    29 | > applyN 5 (twice (adder 1)) 0
           ⧩
           10

    30 | > mk 1 2
           ⧩
           12

    31 | > applyN 4 (mk 2) 0
           ⧩
           80

    32 | > sumWith (x -> x * x) 100
           ⧩
           328350

    33 | > sumWith (compose2 (mk 1) (adder 1)) 10
           ⧩
           155
```

## Bits, booleans, numeric matches and top-level values

Counting bits, `not` on booleans that are values rather than branch conditions, a function
whose body starts with a match on a number, and top-level values that are evaluated once
when they are loaded.

``` unison
use Nat + - * / == < > <= >=

bits : Nat -> Nat
bits n =
  go acc i =
    if Nat.eq i n then acc
    else go (acc + Nat.leadingZeros i + Nat.trailingZeros i + Nat.popCount i) (i + 1)
  go 0 0

structural type Flags = Flags Boolean Boolean

countFlags : Nat -> Nat
countFlags n =
  step = cases Flags a b -> Flags (Boolean.not b) a
  go f acc i =
    if Nat.eq i n then acc
    else match f with
      Flags a b -> go (step f) (if Boolean.not a then acc + 1 else if b then acc + 2 else acc) (i + 1)
  go (Flags true false) 0 0

fibMatch : Nat -> Nat
fibMatch = cases
  0 -> 0
  1 -> 1
  n -> fibMatch (n - 1) + fibMatch (n - 2)

limit : Nat
limit = 3 * 1000 + 7

banner : Text
banner = "ab" Text.++ "cd"

useTop : Nat -> Nat
useTop n =
  go acc i = if i == n then acc else go (acc + limit + Text.size banner) (i + 1)
  go 0 0

-- a function that returns a function, called with more arguments than it
-- takes: the extra one is applied to its result
choose : Nat -> Nat -> Nat
choose x = if x == 0 then (y -> y + 1) else (y -> y * 2)

overApply : Nat -> Nat
overApply n =
  go acc i = if i == n then acc else go (acc + choose (Nat.mod i 2) i) (i + 1)
  go 0 0

> bits 1000
> countFlags 1000
> fibMatch 20
> useTop 1000
> (limit, banner)
> overApply 1000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Flags

  + banner     : Text
  + bits       : Nat -> Nat
  + choose     : Nat -> Nat -> Nat
  + countFlags : Nat -> Nat
  + fibMatch   : Nat -> Nat
  + limit      : Nat
  + overApply  : Nat -> Nat
  + useTop     : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    48 | > bits 1000
           ⧩
           61010

    49 | > countFlags 1000
           ⧩
           1000

    50 | > fibMatch 20
           ⧩
           6765

    51 | > useTop 1000
           ⧩
           3011000

    52 | > (limit, banner)
           ⧩
           (3007, "abcd")

    53 | > overApply 1000
           ⧩
           750000
```

## Lists

List primitives are native (C helpers that read and build the list's structure directly):
size, the views at both ends that pattern matching uses, cons, snoc, `List.at`, append,
take, drop, the patterns that split off a fixed number of elements, and list literals.

``` unison
use Nat + - * / == < > <= >=
use List +: :+

build : Nat -> [Nat]
build n =
  go acc i = if i == n then acc else go (acc :+ i) (i + 1)
  go [] 0

buildFront : Nat -> [Nat]
buildFront n =
  go acc i = if i == n then acc else go (i +: acc) (i + 1)
  go [] 0

sumFront : [Nat] -> Nat
sumFront l =
  go acc = cases
    [] -> acc
    h +: t -> go (acc + h) t
  go 0 l

sumBack : [Nat] -> Nat
sumBack l =
  go acc = cases
    [] -> acc
    i :+ x -> go (acc + x) i
  go 0 l

sumAt : [Nat] -> Nat
sumAt l =
  n = List.size l
  go acc i =
    if Nat.eq i n then acc
    else match List.at i l with
      Some x -> go (acc + x * (Nat.mod i 3)) (i + 1)
      None -> acc
  go 0 0

-- a queue: add at the back, take from the front, at a steady size
queue : Nat -> Nat -> Nat
queue size n =
  go q acc i =
    if i == n then acc + List.size q
    else match q :+ i with
      h +: t -> go t (acc + h) (i + 1)
      [] -> acc
  go (build size) 0 0

-- small lists, both ends at once, and indices out of range
edges : Nat -> Nat
edges n =
  one = cases
    [] -> 0
    [x] -> x
    x +: (rest :+ y) -> x * 2 + y + List.size rest
  go acc i =
    if Nat.eq i n then acc
    else
      l = build (Nat.mod i 13)
      missing = match List.at i l with
        None -> 1
        Some _ -> 0
      go (acc + one l + missing) (i + 1)
  go 0 0

-- lists the interpreter made in other ways: appended and cut
mixed : Nat -> Nat
mixed n =
  l = build n List.++ buildFront n
  m = List.drop 37 (List.take (n + 100) l)
  sumFront m + sumBack m + sumAt m + List.size m

-- elements that are boxed values
pairs : Nat -> Nat
pairs n =
  go acc i = if i == n then acc else go (acc :+ ("ab", i)) (i + 1)
  walk acc = cases
    [] -> acc
    (t, k) +: rest -> walk (acc + Text.size t + k) rest
  walk 0 (go [] 0)

-- take, drop and append in a loop, each on the result of the last
chop : Nat -> Nat
chop n =
  go l acc i =
    if i == n then acc + List.size l
    else
      k = Nat.mod (i * 7) (List.size l + 1)
      a = List.take k l
      b = List.drop k l
      l' = (b :+ i) List.++ a
      next = if List.size l' > 600 then List.drop 100 l' List.++ List.take 3 l' else l'
      go next (acc + List.size a * 3 + sumFront (List.take 2 b)) (i + 1)
  go (build 50) 0 0

-- doubling by append, then cuts in the middle
doubled : Nat -> Nat
doubled n =
  go l i = if i == n then l else go (l List.++ (i +: l)) (i + 1)
  l = go [1, 2, 3] 0
  m = List.take (List.size l / 2 + 17) (List.drop (List.size l / 3) l)
  List.size l + sumFront m + sumBack m + sumAt (List.take 5000 m)

-- patterns that split off a fixed number of elements, and literals
splits : Nat -> Nat
splits n =
  step = cases
    [a, b, c] ++ rest -> (a + b * 2 + c * 3, rest)
    rest -> (List.size rest, [])
  back = cases
    rest ++ [y, z] -> y * 5 + z + List.size rest
    _ -> 0
  go l acc =
    match step l with
      (v, []) -> acc + v
      (v, rest) -> go rest (acc + v + back rest)
  go (build n List.++ [n, n + 1, n + 2, n + 3]) 0

> sumFront (build 100000)
> sumBack (buildFront 100000)
> sumAt (build 30000)
> queue 10 100000
> queue 3000 100000
> edges 2000
> mixed 5000
> pairs 20000
> (sumFront [], sumBack [], sumAt [], List.at 0 [1, 2, 3], List.at 3 [1, 2, 3])
> chop 20000
> doubled 14
> splits 30000
> (List.take 2 [1, 2, 3], List.drop 2 [1, 2, 3], [1, 2] List.++ [3], List.take 0 [1], List.drop 5 [1], [] List.++ [7])
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + build      : Nat -> [Nat]
  + buildFront : Nat -> [Nat]
  + chop       : Nat -> Nat
  + doubled    : Nat -> Nat
  + edges      : Nat -> Nat
  + mixed      : Nat -> Nat
  + pairs      : Nat -> Nat
  + queue      : Nat -> Nat -> Nat
  + splits     : Nat -> Nat
  + sumAt      : [Nat] -> Nat
  + sumBack    : [Nat] -> Nat
  + sumFront   : [Nat] -> Nat

  Run `update` to apply these changes to your codebase.

    118 | > sumFront (build 100000)
            ⧩
            4999950000

    119 | > sumBack (buildFront 100000)
            ⧩
            4999950000

    120 | > sumAt (build 30000)
            ⧩
            450005000

    121 | > queue 10 100000
            ⧩
            4998950110

    122 | > queue 3000 100000
            ⧩
            4708953000

    123 | > edges 2000
            ⧩
            20594

    124 | > mixed 5000
            ⧩
            38978757

    125 | > pairs 20000
            ⧩
            200030000

    126 | > (sumFront [], sumBack [], sumAt [], List.at 0 [1, 2, 3], List.at 3 [1, 2, 3])
            ⧩
            (0, 0, 0, Some 1, None)

    127 | > chop 20000
            ⧩
            394214577

    128 | > doubled 14
            ⧩
            189028

    129 | > splits 30000
            ⧩
            2850305009

    130 | > (List.take 2 [1, 2, 3], List.drop 2 [1, 2, 3], [1, 2] List.++ [3], List.take 0 [1], List.drop 5 [1], [] List.++ [7])
            ⧩
            ([1, 2], [3], [1, 2, 3], [], [], [7])
```

## Text

Text primitives with native versions (C helpers that read and build the rope directly):
size, `++`, take, drop and equality.

``` unison
use Nat + - * / == < > <= >=
use Text ++

grow : Nat -> Text
grow n =
  go acc i = if i == n then acc else go (acc ++ "ab") (i + 1)
  go "" 0

growFront : Nat -> Text
growFront n =
  go acc i = if i == n then acc else go (Nat.toText i ++ acc) (i + 1)
  go "" 0

-- drop a character at a time
eat : Text -> Nat
eat t =
  go acc t = if Text.size t == 0 then acc else go (acc + Text.size t) (Text.drop 1 t)
  go 0 t

-- take all but the last character, until nothing is left
chop : Text -> Nat
chop t =
  go acc t = if Text.eq t "" then acc else go (acc + 1) (Text.take (Text.size t - 1) t)
  go 0 t

-- a text cut in two and put together again is the same text, in other chunks
recut : Text -> Nat
recut t =
  n = Text.size t
  go acc i =
    if i > n then acc
    else
      u = Text.take i t ++ Text.drop i t
      go (if Text.eq u t then acc + 1 else acc) (i + 7)
  go 0 0

-- characters of more than one byte
wide : Nat -> Nat
wide n =
  go acc t i =
    if i == n then acc + Text.size t
    else go (acc + Text.size (Text.take 3 t)) (Text.drop 1 (t ++ "é€😀")) (i + 1)
  go 0 "λx" 0

-- texts of many chunks appended to each other and cut in the middle
weave : Nat -> Nat
weave n =
  go acc t i =
    if i == n then acc + Text.size t
    else
      u = t ++ Nat.toText i ++ t
      m = Text.size u / 3
      mid = Text.take m (Text.drop m u)
      same = Text.eq (Text.take m u ++ Text.drop m u) u
      go (acc + Text.size mid + (if same then 1 else 0)) (if Text.size u > 100000 then mid else u) (i + 1)
  go 0 (grow 100) 0

-- for printing the results of the operations that return structures
restSize : Optional (Char, Text) -> Nat
restSize = cases
  None -> 0
  Some (c, rest) -> Text.size rest

floatOpt : Optional Float -> Text
floatOpt = cases
  None -> "none"
  Some f -> Float.toText f

decodedSize : Either Failure Text -> Int
decodedSize = cases
  Left _ -> -1
  Right t -> Nat.toInt (Text.size t)

> Text.size (grow 20000)
> Text.size (growFront 3000)
> weave 60
> eat (grow 2000)
> chop (growFront 500)
> recut (grow 3000)
> wide 5000
> (Text.eq (grow 3) "ababab", Text.eq (grow 3) "ababa", Text.take 2 (grow 40), Text.drop 77 (grow 40), "" ++ "", Text.size "")
> (Text.uncons "", Text.uncons "héllo", Text.unsnoc "héllo", restSize (Text.uncons (grow 100)))
> (Text.toCharList "a€😀", Text.fromCharList (List.cons ?a (Text.toCharList (grow 40))), Text.size (Text.fromCharList (Text.toCharList (grow 1000))))
> (Nat.toText 0, Nat.toText 18446744073709551615, Int.toText -9223372036854775808, Int.toText +42, Float.toText 0.1, Float.toText 1.0e7, Float.toText 123456.789, Float.toText -0.0, Float.toText (Float.fromRepresentation 9218868437227405312))
> (Nat.fromText "42", Nat.fromText "-1", Nat.fromText "x", Int.fromText "-42", Int.fromText "+7", Int.fromText "9223372036854775808", Float.fromText "1.5e3", Float.fromText "1.", floatOpt (Float.fromText "NaN"))
> (Text.indexOf "lo" "héllo", Text.indexOf "z" "héllo", Text.indexOf "" "abc", Text.indexOf "ba" (grow 2000), "abc" Universal.< "abd", "b" Universal.<= "abc", Universal.compare "abc" "abd", Universal.compare (grow 3) (grow 3))
> (Text.repeat 3 "ab", Text.size (Text.repeat 1000 "héllo"), Text.reverse "héllo", Text.eq (Text.reverse (Text.drop 1 (grow 100))) (Text.take 198 (grow 100)), Text.toUppercase "héllo wörld", Text.toUppercase "hello", Text.toLowercase "HELLO", Char.toText ?€)
> (Text.toUtf8 "héllo", decodedSize (Text.fromUtf8.impl (Text.toUtf8 (grow 100))), decodedSize (Text.fromUtf8.impl 0xsff))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + chop        : Text -> Nat
  + decodedSize : Either Failure Text -> Int
  + eat         : Text -> Nat
  + floatOpt    : Optional Float -> Text
  + grow        : Nat -> Text
  + growFront   : Nat -> Text
  + recut       : Text -> Nat
  + restSize    : Optional (Char, Text) -> Nat
  + weave       : Nat -> Nat
  + wide        : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    74 | > Text.size (grow 20000)
           ⧩
           40000

    75 | > Text.size (growFront 3000)
           ⧩
           10890

    76 | > weave 60
           ⧩
           2142964

    77 | > eat (grow 2000)
           ⧩
           8002000

    78 | > chop (growFront 500)
           ⧩
           1390

    79 | > recut (grow 3000)
           ⧩
           858

    80 | > wide 5000
           ⧩
           25001

    81 | > (Text.eq (grow 3) "ababab", Text.eq (grow 3) "ababa", Text.take 2 (grow 40), Text.drop 77 (grow 40), "" ++ "", Text.size "")
           ⧩
           (true, false, "ab", "bab", "", 0)

    82 | > (Text.uncons "", Text.uncons "héllo", Text.unsnoc "héllo", restSize (Text.uncons (grow 100)))
           ⧩
           (None, Some (?h, "éllo"), Some ("héll", ?o), 199)

    83 | > (Text.toCharList "a€😀", Text.fromCharList (List.cons ?a (Text.toCharList (grow 40))), Text.size (Text.fromCharList (Text.toCharList (grow 1000))))
           ⧩
           ( [?a, ?€, ?😀]
           , "aabababababababababababababababababababababababababababababababababababababababab"
           , 2000
           )

    84 | > (Nat.toText 0, Nat.toText 18446744073709551615, Int.toText -9223372036854775808, Int.toText +42, Float.toText 0.1, Float.toText 1.0e7, Float.toText 123456.789, Float.toText -0.0, Float.toText (Float.fromRepresentation 9218868437227405312))
           ⧩
           ( "0"
           , "18446744073709551615"
           , "-9223372036854775808"
           , "42"
           , "0.1"
           , "1.0e7"
           , "123456.789"
           , "-0.0"
           , "Infinity"
           )

    85 | > (Nat.fromText "42", Nat.fromText "-1", Nat.fromText "x", Int.fromText "-42", Int.fromText "+7", Int.fromText "9223372036854775808", Float.fromText "1.5e3", Float.fromText "1.", floatOpt (Float.fromText "NaN"))
           ⧩
           ( Some 42
           , None
           , None
           , Some -42
           , Some +7
           , None
           , Some 1500.0
           , None
           , "NaN"
           )

    86 | > (Text.indexOf "lo" "héllo", Text.indexOf "z" "héllo", Text.indexOf "" "abc", Text.indexOf "ba" (grow 2000), "abc" Universal.< "abd", "b" Universal.<= "abc", Universal.compare "abc" "abd", Universal.compare (grow 3) (grow 3))
           ⧩
           (Some 3, None, Some 0, Some 1, true, false, -1, +0)

    87 | > (Text.repeat 3 "ab", Text.size (Text.repeat 1000 "héllo"), Text.reverse "héllo", Text.eq (Text.reverse (Text.drop 1 (grow 100))) (Text.take 198 (grow 100)), Text.toUppercase "héllo wörld", Text.toUppercase "hello", Text.toLowercase "HELLO", Char.toText ?€)
           ⧩
           ( "ababab"
           , 5000
           , "olléh"
           , false
           , "HÉLLO WÖRLD"
           , "HELLO"
           , "hello"
           , "€"
           )

    88 | > (Text.toUtf8 "héllo", decodedSize (Text.fromUtf8.impl (Text.toUtf8 (grow 100))), decodedSize (Text.fromUtf8.impl 0xsff))
           ⧩
           (0xs68c3a96c6c6f, +200, -1)
```

## Bytes

Bytes is the same rope as Text with chunks of bytes, and has native versions of the same
operations (size, `++`, take, drop) plus `Bytes.at`, `Bytes.flatten` and `==` on two Bytes or
two Text values (universal equality, `Universal.==`).

``` unison
use Nat + - * / == < > <= >=
use Bytes ++

bgrow : Nat -> Bytes
bgrow n =
  go acc i = if i == n then acc else go (acc ++ Bytes.fromList [Nat.mod i 251, 255]) (i + 1)
  go Bytes.empty 0

bgrowFront : Nat -> Bytes
bgrowFront n =
  go acc i = if i == n then acc else go (Bytes.fromList [Nat.mod i 251, Nat.mod (i + 1) 251, Nat.mod (i + 2) 251] ++ acc) (i + 1)
  go Bytes.empty 0

-- drop a byte at a time, adding up the bytes seen
beat : Bytes -> Nat
beat b =
  go acc b = match Bytes.at 0 b with
    None -> acc
    Some x -> go (acc + x) (Bytes.drop 1 b)
  go 0 b

-- take all but the last byte, until nothing is left
bchop : Bytes -> Nat
bchop b =
  go acc b = if b Universal.== Bytes.empty then acc else go (acc + Bytes.size b) (Bytes.take (Bytes.size b - 1) b)
  go 0 b

-- a bytes cut in two and put together again is the same bytes, in other chunks
brecut : Bytes -> Nat
brecut b =
  n = Bytes.size b
  go acc i =
    if i > n then acc
    else
      u = Bytes.take i b ++ Bytes.drop i b
      go (if u Universal.== b then acc + 1 else acc) (i + 7)
  go 0 0

-- bytes of many chunks appended to each other and cut in the middle
bweave : Nat -> Nat
bweave n =
  go acc b i =
    if i == n then acc + Bytes.size b
    else
      u = b ++ Bytes.fromList [i] ++ b
      m = Bytes.size u / 3
      mid = Bytes.take m (Bytes.drop m u)
      same = (Bytes.take m u ++ Bytes.drop m u) Universal.== u
      go (acc + Bytes.size mid + (if same then 1 else 0)) (if Bytes.size u > 100000 then mid else u) (i + 1)
  go 0 (bgrow 100) 0

-- every byte of a flattened bytes, weighted by its position
bsum : Bytes -> Nat
bsum b =
  f = Bytes.flatten b
  byte i = match Bytes.at i f with
    None -> 0
    Some x -> x
  go acc i = if i == Bytes.size f then acc else go (acc + byte i * (i + 1)) (i + 1)
  go 0 0

decodedBytes : Either Text Bytes -> Int
decodedBytes = cases
  Left _ -> -1
  Right b -> Nat.toInt (Bytes.size b)

> Bytes.size (bgrow 20000)
> Bytes.size (bgrowFront 3000)
> bweave 60
> beat (bgrow 2000)
> bchop (bgrowFront 500)
> brecut (bgrow 3000)
> bsum (bgrow 1000)
> bsum (bgrowFront 1000)
> (bgrow 3 Universal.== Bytes.fromList [0, 255, 1, 255, 2, 255], bgrow 3 Universal.== Bytes.fromList [0, 255, 1, 255, 2], Bytes.toList (Bytes.take 3 (bgrow 40)), Bytes.toList (Bytes.drop 77 (bgrow 40)))
> (Bytes.at 5 (bgrow 3), Bytes.at 6 (bgrow 3), Bytes.size (Bytes.flatten (bgrow 400)), Bytes.flatten (bgrow 400) Universal.== bgrow 400, ("ab" Text.++ "c") Universal.== "abc", "abc" Universal.== "abd", Bytes.empty Universal.== Bytes.empty)
> (##Bytes.toList (bgrow 3), ##Bytes.fromList (##Bytes.toList (bgrow 300)) Universal.== bgrow 300, ##Bytes.indexOf 0xs01ff (bgrow 3), ##Bytes.indexOf 0xsffff (bgrow 3), Universal.compare 0xs0102 0xs0103, Universal.compare (bgrow 5) (bgrow 5), 0xs01 Universal.< 0xs0100)
> (##Bytes.decodeNat16be 0xs0102ff, ##Bytes.decodeNat32le 0xs01020304, ##Bytes.decodeNat64be (bgrow 2), ##Bytes.decodeNat64be 0xs01, ##Bytes.encodeNat16be 258, ##Bytes.encodeNat64le 1, ##Bytes.encodeNat32be 4294967295)
> (##Bytes.toBase16 0xs00ff10, ##Bytes.toBase32 0xs68656c6c6f, ##Bytes.toBase64 0xs68656c6c6f, ##Bytes.toBase64UrlUnpadded 0xsfbff, ##Bytes.fromBase16 0xs30306666, ##Bytes.fromBase16 0xs303066, decodedBytes (##Bytes.fromBase64 (##Bytes.toBase64 (bgrow 50))), decodedBytes (##Bytes.fromBase32 (##Bytes.toBase32 (bgrow 7))))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bchop        : Bytes -> Nat
  + beat         : Bytes -> Nat
  + bgrow        : Nat -> Bytes
  + bgrowFront   : Nat -> Bytes
  + brecut       : Bytes -> Nat
  + bsum         : Bytes -> Nat
  + bweave       : Nat -> Nat
  + decodedBytes : Either Text Bytes -> Int

  Run `update` to apply these changes to your codebase.

    67 | > Bytes.size (bgrow 20000)
           ⧩
           40000

    68 | > Bytes.size (bgrowFront 3000)
           ⧩
           9000

    69 | > bweave 60
           ⧩
           2142485

    70 | > beat (bgrow 2000)
           ⧩
           759028

    71 | > bchop (bgrowFront 500)
           ⧩
           1125750

    72 | > brecut (bgrow 3000)
           ⧩
           858

    73 | > bsum (bgrow 1000)
           ⧩
           389807014

    74 | > bsum (bgrowFront 1000)
           ⧩
           516376967

    75 | > (bgrow 3 Universal.== Bytes.fromList [0, 255, 1, 255, 2, 255], bgrow 3 Universal.== Bytes.fromList [0, 255, 1, 255, 2], Bytes.toList (Bytes.take 3 (bgrow 40)), Bytes.toList (Bytes.drop 77 (bgrow 40)))
           ⧩
           (true, false, [0, 255, 1], [255, 39, 255])

    76 | > (Bytes.at 5 (bgrow 3), Bytes.at 6 (bgrow 3), Bytes.size (Bytes.flatten (bgrow 400)), Bytes.flatten (bgrow 400) Universal.== bgrow 400, ("ab" Text.++ "c") Universal.== "abc", "abc" Universal.== "abd", Bytes.empty Universal.== Bytes.empty)
           ⧩
           (Some 255, None, 800, true, true, false, true)

    77 | > (##Bytes.toList (bgrow 3), ##Bytes.fromList (##Bytes.toList (bgrow 300)) Universal.== bgrow 300, ##Bytes.indexOf 0xs01ff (bgrow 3), ##Bytes.indexOf 0xsffff (bgrow 3), Universal.compare 0xs0102 0xs0103, Universal.compare (bgrow 5) (bgrow 5), 0xs01 Universal.< 0xs0100)
           ⧩
           ( [0, 255, 1, 255, 2, 255]
           , true
           , Some 2
           , None
           , -1
           , +0
           , true
           )

    78 | > (##Bytes.decodeNat16be 0xs0102ff, ##Bytes.decodeNat32le 0xs01020304, ##Bytes.decodeNat64be (bgrow 2), ##Bytes.decodeNat64be 0xs01, ##Bytes.encodeNat16be 258, ##Bytes.encodeNat64le 1, ##Bytes.encodeNat32be 4294967295)
           ⧩
           ( Some (258, 0xsff)
           , Some (67305985, 0xs)
           , None
           , None
           , 0xs0102
           , 0xs0100000000000000
           , 0xsffffffff
           )

    79 | > (##Bytes.toBase16 0xs00ff10, ##Bytes.toBase32 0xs68656c6c6f, ##Bytes.toBase64 0xs68656c6c6f, ##Bytes.toBase64UrlUnpadded 0xsfbff, ##Bytes.fromBase16 0xs30306666, ##Bytes.fromBase16 0xs303066, decodedBytes (##Bytes.fromBase64 (##Bytes.toBase64 (bgrow 50))), decodedBytes (##Bytes.fromBase32 (##Bytes.toBase32 (bgrow 7))))
           ⧩
           ( 0xs303066663130
           , 0xs4e42535759334450
           , 0xs614756736247383d
           , 0xs2d5f38
           , Right 0xs00ff
           , Left "base16: input: invalid length"
           , +100
           , +14
           )
```

## Partial applications

A function with some of its arguments supplied is a value built natively (the `Name`
instruction): a copy of the function's closure with the arguments added.

``` unison
use Nat + - * / == < > <= >=

add3 : Nat -> Nat -> Nat -> Nat
add3 a b c = a + b * 2 + c * 3

applyAll : (Nat -> Nat) -> Nat -> Nat -> Nat
applyAll f n acc = if n == 0 then acc else applyAll f (n - 1) (f acc)

-- a closure made fresh in every iteration, from a known function
fresh : Nat -> Nat
fresh n =
  go acc i = if i == n then acc else
    f = add3 i (i + 1)
    go (acc + f 1 + f 2) (i + 1)
  go 0 0

-- more arguments added to a function value that already holds some
more : Nat -> Nat
more n =
  go acc i = if i == n then acc else
    f = add3 i
    g = f (i + 1)
    go (acc + g i + applyAll g 3 0) (i + 1)
  go 0 0

-- five captured values: more than the helper takes, so the interpreter builds it
five : Nat -> Nat
five n =
  go acc i = if i == n then acc else
    a = i + 1
    b = i + 2
    c = i + 3
    d = i + 4
    f x = a + b + c + d + i + x
    go (acc + applyAll f 2 0) (i + 1)
  go 0 0

> fresh 20000
> more 5000
> five 3000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + add3     : Nat -> Nat -> Nat -> Nat
  + applyAll : (Nat ->{g} Nat) -> Nat -> Nat ->{g} Nat
  + five     : Nat -> Nat
  + fresh    : Nat -> Nat
  + more     : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    38 | > fresh 20000
           ⧩
           1200200000

    39 | > more 5000
           ⧩
           562527500

    40 | > five 3000
           ⧩
           45045000
```

## Floats, pow and representation casts

Every Float operation is native, as are `pow` on Int and Nat and the type-tag casts behind
`Nat.toInt`, `Char.toNat` and `Float.toRepresentation`. The results must be bit-for-bit the
interpreter's (Haskell's), including signed zeros, NaN, Haskell's half-to-even `round`, its
`min`/`max` with NaN, and its own `atan2`.

``` unison
use Nat + - * / == < > <= >=

fsum : Nat -> Float
fsum n =
  go acc i = if i == n then acc else go (acc Float.+ Float.sqrt (Nat.toFloat i)) (i + 1)
  go 0.0 0

> (1.5 Float.+ 2.25, 1.5 Float.- 2.25, 1.5 Float.* 2.25, 1.5 Float./ 2.25, 1.0 Float./ 0.0, -1.0 Float./ 0.0, 0.0 Float./ 0.0, -0.0 Float.* 1.0)
> (Float.lt 1.0 2.0, Float.gt 1.0 2.0, Float.lteq 2.0 2.0, Float.gteq 1.0 2.0, Float.eq 2.0 2.0, Float.eq (0.0 Float./ 0.0) (0.0 Float./ 0.0), Float.lteq (0.0 Float./ 0.0) 1.0, Float.gt (0.0 Float./ 0.0) 1.0)
> (Float.min 1.0 2.0, Float.max 1.0 2.0, Float.min (0.0 Float./ 0.0) 1.0, Float.min 1.0 (0.0 Float./ 0.0), Float.max (0.0 Float./ 0.0) 1.0, Float.max 1.0 (0.0 Float./ 0.0), Float.min -0.0 0.0, Float.max 0.0 -0.0)
> (Float.ceiling 2.3, Float.ceiling -2.3, Float.floor 2.3, Float.floor -2.3, Float.truncate 2.7, Float.truncate -2.7, Float.round 2.5, Float.round 3.5, Float.round -2.5, Float.round 0.5, Float.round -0.4, Float.round 2.4999)
> (Float.abs -3.5, Float.abs 3.5, Float.abs -0.0, Float.sqrt 2.0, Float.sqrt -1.0, Float.exp 1.0, Float.log 10.0, Float.log 0.0, Float.logBase 2.0 1024.0, Float.logBase 10.0 0.001, Float.pow 2.0 0.5, Float.pow -8.0 (1.0 Float./ 3.0))
> (Float.cos 1.0, Float.sin 1.0, Float.tan 1.0, Float.acos 0.5, Float.asin 0.5, Float.atan 0.5, Float.cosh 0.5, Float.sinh 0.5, Float.tanh 0.5, Float.asinh 1.0, Float.acosh 2.0, Float.atanh 0.5)
> (Float.atan2 1.0 1.0, Float.atan2 1.0 -1.0, Float.atan2 -1.0 -1.0, Float.atan2 -1.0 1.0, Float.atan2 1.0 0.0, Float.atan2 -1.0 0.0, Float.atan2 0.0 -1.0, Float.atan2 -0.0 -1.0, Float.atan2 0.0 0.0, Float.atan2 -0.0 0.0, Float.atan2 0.0 -0.0, Float.atan2 -0.0 -0.0, Float.atan2 (0.0 Float./ 0.0) 1.0)
> (Nat.toFloat 3, Nat.toFloat 18446744073709551615, Int.toFloat -3, Int.toFloat +9007199254740993, fsum 1000)
> (Int.pow +3 4, Int.pow -2 3, Int.pow -2 63, Int.pow +7 0, Int.pow +0 0, Nat.pow 3 4, Nat.pow 2 63, Nat.pow 2 64, Nat.pow 3 41, Nat.pow 0 0)
> (Nat.toInt 5, Nat.toInt 18446744073709551615, Int.toRepresentation -1, Int.fromRepresentation 18446744073709551615, Char.toNat ?a, Char.fromNat 955, Float.toRepresentation 1.0, Float.toRepresentation -0.0, Float.fromRepresentation 4611686018427387904)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + fsum : Nat -> Float

  Run `update` to apply these changes to your codebase.

    8 | > (1.5 Float.+ 2.25, 1.5 Float.- 2.25, 1.5 Float.* 2.25, 1.5 Float./ 2.25, 1.0 Float./ 0.0, -1.0 Float./ 0.0, 0.0 Float./ 0.0, -0.0 Float.* 1.0)
          ⧩
          ( 3.75
          , -0.75
          , 3.375
          , 0.6666666666666666
          , Infinity
          , -Infinity
          , NaN
          , -0.0
          )

    9 | > (Float.lt 1.0 2.0, Float.gt 1.0 2.0, Float.lteq 2.0 2.0, Float.gteq 1.0 2.0, Float.eq 2.0 2.0, Float.eq (0.0 Float./ 0.0) (0.0 Float./ 0.0), Float.lteq (0.0 Float./ 0.0) 1.0, Float.gt (0.0 Float./ 0.0) 1.0)
          ⧩
          (true, false, true, false, true, false, false, false)

    10 | > (Float.min 1.0 2.0, Float.max 1.0 2.0, Float.min (0.0 Float./ 0.0) 1.0, Float.min 1.0 (0.0 Float./ 0.0), Float.max (0.0 Float./ 0.0) 1.0, Float.max 1.0 (0.0 Float./ 0.0), Float.min -0.0 0.0, Float.max 0.0 -0.0)
           ⧩
           (1.0, 2.0, 1.0, NaN, NaN, 1.0, -0.0, -0.0)

    11 | > (Float.ceiling 2.3, Float.ceiling -2.3, Float.floor 2.3, Float.floor -2.3, Float.truncate 2.7, Float.truncate -2.7, Float.round 2.5, Float.round 3.5, Float.round -2.5, Float.round 0.5, Float.round -0.4, Float.round 2.4999)
           ⧩
           (+3, -2, +2, -3, +2, -2, +2, +4, -2, +0, +0, +2)

    12 | > (Float.abs -3.5, Float.abs 3.5, Float.abs -0.0, Float.sqrt 2.0, Float.sqrt -1.0, Float.exp 1.0, Float.log 10.0, Float.log 0.0, Float.logBase 2.0 1024.0, Float.logBase 10.0 0.001, Float.pow 2.0 0.5, Float.pow -8.0 (1.0 Float./ 3.0))
           ⧩
           ( 3.5
           , 3.5
           , 0.0
           , 1.4142135623730951
           , NaN
           , 2.718281828459045
           , 2.302585092994046
           , -Infinity
           , 10.0
           , -2.9999999999999996
           , 1.4142135623730951
           , NaN
           )

    13 | > (Float.cos 1.0, Float.sin 1.0, Float.tan 1.0, Float.acos 0.5, Float.asin 0.5, Float.atan 0.5, Float.cosh 0.5, Float.sinh 0.5, Float.tanh 0.5, Float.asinh 1.0, Float.acosh 2.0, Float.atanh 0.5)
           ⧩
           ( 0.5403023058681398
           , 0.8414709848078965
           , 1.557407724654902
           , 1.0471975511965976
           , 0.5235987755982988
           , 0.46364760900080615
           , 1.1276259652063807
           , 0.5210953054937474
           , 0.46211715726000974
           , 0.881373587019543
           , 1.3169578969248166
           , 0.5493061443340549
           )

    14 | > (Float.atan2 1.0 1.0, Float.atan2 1.0 -1.0, Float.atan2 -1.0 -1.0, Float.atan2 -1.0 1.0, Float.atan2 1.0 0.0, Float.atan2 -1.0 0.0, Float.atan2 0.0 -1.0, Float.atan2 -0.0 -1.0, Float.atan2 0.0 0.0, Float.atan2 -0.0 0.0, Float.atan2 0.0 -0.0, Float.atan2 -0.0 -0.0, Float.atan2 (0.0 Float./ 0.0) 1.0)
           ⧩
           ( 0.7853981633974483
           , 2.356194490192345
           , -2.356194490192345
           , -0.7853981633974483
           , 1.5707963267948966
           , -1.5707963267948966
           , 3.141592653589793
           , -3.141592653589793
           , 0.0
           , -0.0
           , 3.141592653589793
           , -3.141592653589793
           , NaN
           )

    15 | > (Nat.toFloat 3, Nat.toFloat 18446744073709551615, Int.toFloat -3, Int.toFloat +9007199254740993, fsum 1000)
           ⧩
           ( 3.0
           , 1.8446744073709552e19
           , -3.0
           , 9.007199254740992e15
           , 21065.833110879048
           )

    16 | > (Int.pow +3 4, Int.pow -2 3, Int.pow -2 63, Int.pow +7 0, Int.pow +0 0, Nat.pow 3 4, Nat.pow 2 63, Nat.pow 2 64, Nat.pow 3 41, Nat.pow 0 0)
           ⧩
           ( +81
           , -8
           , -9223372036854775808
           , +1
           , +1
           , 81
           , 9223372036854775808
           , 0
           , 18026252303461234787
           , 1
           )

    17 | > (Nat.toInt 5, Nat.toInt 18446744073709551615, Int.toRepresentation -1, Int.fromRepresentation 18446744073709551615, Char.toNat ?a, Char.fromNat 955, Float.toRepresentation 1.0, Float.toRepresentation -0.0, Float.fromRepresentation 4611686018427387904)
           ⧩
           ( +5
           , -1
           , 18446744073709551615
           , -1
           , 97
           , ?λ
           , 4607182418800017408
           , 9223372036854775808
           , 2.0
           )
```

## Arrays, byte arrays, refs and tickets

Every array builtin is native (mutable and immutable, pointer and byte arrays: sizes, reads
of all widths and both byte orders, writes, `copyTo!`, `freeze` and `freeze!`, `toBytes` and
`fromBytes`, the `Scope` and `IO` constructors), as are `Scope.ref`/`IO.ref`, `Ref.readForCas`,
`Ticket.read` and `Ref.cas`. The bounds checks are the interpreter's (the failing calls below
are left to it, which raises).

``` unison
use Nat + - * / == < > <= >=

catchF : '{IO, Exception} a ->{IO} Either Text a
catchF c = handle c() with cases
  { a } -> Right a
  { Exception.raise f -> _ } -> match f with Failure _ msg _ -> Left msg

fillSq : MutableArray {IO} Nat -> Nat ->{IO, Exception} ()
fillSq arr i =
  if i == MutableArray.size arr then ()
  else
    MutableArray.write arr i (i * i)
    fillSq arr (i + 1)

sumM : MutableArray {IO} Nat -> Nat -> Nat ->{IO, Exception} Nat
sumM arr i acc =
  if i == MutableArray.size arr then acc else sumM arr (i + 1) (acc + MutableArray.read arr i)

sumI : ImmutableArray Nat -> Nat -> Nat ->{Exception} Nat
sumI arr i acc =
  if i == ImmutableArray.size arr then acc else sumI arr (i + 1) (acc + ImmutableArray.read arr i)

pointerArrays : '{IO} (Either Text (Nat, Nat, Nat, Nat, Nat, Nat, Nat), Either Text Nat, Either Text (), Either Text Nat, Either Text Nat)
pointerArrays = do
  m = IO.arrayOf 0 8
  big = IO.arrayOf 99 12
  (catchF do
     fillSq m 0
     im = MutableArray.freeze m 2 4
     MutableArray.copyTo! big 1 m 0 8
     ImmutableArray.copyTo! big 9 im 1 3
     im2 = MutableArray.freeze! m
     empty = MutableArray.freeze m 5 0
     (MutableArray.size m, ImmutableArray.size im, sumI im 0 0, sumM big 0 0, ImmutableArray.read im2 7, ImmutableArray.size empty, ImmutableArray.size im2),
   catchF '(MutableArray.read m 8),
   catchF '(MutableArray.copyTo! big 5 m 0 8),
   catchF '(ImmutableArray.size (MutableArray.freeze m 6 3)),
   catchF '(ImmutableArray.read (MutableArray.freeze m 0 2) 2))

byteReads : MutableByteArray {IO} ->{IO, Exception} [Nat]
byteReads b =
  [ MutableByteArray.read8 b 0, MutableByteArray.read16be b 0, MutableByteArray.read16le b 0,
    MutableByteArray.read24be b 1, MutableByteArray.read24le b 1, MutableByteArray.read32be b 2,
    MutableByteArray.read32le b 2, MutableByteArray.read40be b 3, MutableByteArray.read40le b 3,
    MutableByteArray.read64be b 0, MutableByteArray.read64le b 0, MutableByteArray.read8 b 31 ]

immReads : ImmutableByteArray ->{Exception} [Nat]
immReads b =
  [ ImmutableByteArray.read8 b 0, ImmutableByteArray.read16be b 0, ImmutableByteArray.read16le b 0,
    ImmutableByteArray.read24be b 1, ImmutableByteArray.read24le b 1, ImmutableByteArray.read32be b 2,
    ImmutableByteArray.read32le b 2, ImmutableByteArray.read40be b 3, ImmutableByteArray.read40le b 3,
    ImmutableByteArray.read64be b 0, ImmutableByteArray.read64le b 0, ImmutableByteArray.size b ]

byteArrays : '{IO} (Either Text ([Nat], [Nat], Nat, Bytes, Bytes, [Nat]), Either Text Nat, Either Text Nat, Either Text (), Either Text (), Either Text Bytes, Either Text Nat)
byteArrays = do
  b = IO.bytearrayOf 170 32
  (catchF do
     MutableByteArray.write8 b 0 1
     MutableByteArray.write16be b 1 515
     MutableByteArray.write32le b 3 67305985
     MutableByteArray.write64be b 8 1234605616436508552
     MutableByteArray.write16le b 16 65535
     MutableByteArray.write32be b 18 305419896
     MutableByteArray.write64le b 24 18446744073709551615
     rs = byteReads b
     c = IO.bytearray 16
     MutableByteArray.copyTo! c 4 b 0 12
     MutableByteArray.copyTo! c 0 c 2 4
     frozen = MutableByteArray.freeze b 0 32
     snap = MutableByteArray.freeze c 2 10
     bs = ImmutableByteArray.toBytes frozen 5 11
     back = ImmutableByteArray.fromBytes (bs Bytes.++ ImmutableByteArray.toBytes snap 0 10)
     frozen2 = MutableByteArray.freeze! c
     ImmutableByteArray.copyTo! b 20 frozen2 0 6
     (rs, immReads frozen, MutableByteArray.size b, bs, ImmutableByteArray.toBytes back 0 (ImmutableByteArray.size back), immReads (MutableByteArray.freeze! b)),
   catchF '(MutableByteArray.read64le b 25),
   catchF '(MutableByteArray.read8 b 18446744073709551615),
   catchF '(MutableByteArray.write16be b 31 1),
   catchF '(MutableByteArray.copyTo! b 30 b 0 3),
   catchF '(ImmutableByteArray.toBytes (MutableByteArray.freeze b 0 8) 4 5),
   catchF '(ImmutableByteArray.size (MutableByteArray.freeze b 30 3)))

scoped : '{IO} (Either Text (Nat, Nat, Nat, Nat, Nat, Nat))
scoped = do
  catchF do
    Scope.run do
      a = Scope.array 3
      r = Scope.ref 10
      MutableArray.write a 0 (Ref.read r)
      Ref.write r 20
      MutableArray.write a 1 (Ref.read r)
      MutableArray.write a 2 (MutableArray.read a 0 + MutableArray.read a 1)
      z = Scope.arrayOf 7 4
      ba = Scope.bytearrayOf 9 6
      MutableByteArray.write8 ba 5 200
      p = Scope.pinnedByteArrayOf 1 8
      pm = PinnedByteArray.cast p
      MutableByteArray.write32be pm 0 4278255360
      zero = Scope.bytearray 4
      MutableByteArray.write32le zero 0 0
      (MutableArray.read a 2, MutableArray.read z 3, MutableByteArray.read8 ba 5 + MutableByteArray.read8 ba 0, MutableByteArray.read16be pm 1, MutableByteArray.read32be pm 4, MutableByteArray.size zero)

tickets : '{IO} (Nat, Boolean, Nat, Boolean, Nat, Boolean, Nat)
tickets = do
  r = IO.ref 5
  t = Ref.readForCas r
  ok1 = Ref.cas r t 6
  v1 = Ref.read r
  ok2 = Ref.cas r t 7
  t2 = Ref.readForCas r
  ok3 = Ref.cas r t2 8
  (Ref.Ticket.read t, ok1, v1, ok2, Ref.Ticket.read t2, ok3, Ref.read r)

casLoop : Nat ->{IO} Nat
casLoop n =
  r = IO.ref 0
  go i =
    if i == n then Ref.read r
    else
      t = Ref.readForCas r
      if Ref.cas r t (Ref.Ticket.read t + i) then go (i + 1) else go i
  go 0

casTest : '{IO} Nat
casTest = do casLoop 10000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + byteArrays    : '{IO} ( Either
                      Text
                      ([Nat], [Nat], Nat, Bytes, Bytes, [Nat]),
                      Either Text Nat,
                      Either Text Nat,
                      Either Text (),
                      Either Text (),
                      Either Text Bytes,
                      Either Text Nat)
  + byteReads     : MutableByteArray {IO}
                    ->{IO, Exception} [Nat]
  + casLoop       : Nat ->{IO} Nat
  + casTest       : '{IO} Nat
  + catchF        : '{IO, Exception} a ->{IO} Either Text a
  + fillSq        : MutableArray {IO} Nat
                    -> Nat
                    ->{IO, Exception} ()
  + immReads      : ImmutableByteArray ->{Exception} [Nat]
  + pointerArrays : '{IO} ( Either
                      Text (Nat, Nat, Nat, Nat, Nat, Nat, Nat),
                      Either Text Nat,
                      Either Text (),
                      Either Text Nat,
                      Either Text Nat)
  + scoped        : '{IO} Either
                      Text (Nat, Nat, Nat, Nat, Nat, Nat)
  + sumI          : ImmutableArray Nat
                    -> Nat
                    -> Nat
                    ->{Exception} Nat
  + sumM          : MutableArray {IO} Nat
                    -> Nat
                    -> Nat
                    ->{IO, Exception} Nat
  + tickets       : '{IO} ( Nat,
                      Boolean,
                      Nat,
                      Boolean,
                      Nat,
                      Boolean,
                      Nat)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run pointerArrays

  ( Right (8, 4, 54, 289, 49, 0, 8)
  , Left "MutableArray.read: array index out of bounds"
  , Left "MutableArray.copyTo!: array index out of bounds"
  , Left "MutableArray.freeze: array index out of bounds"
  , Left "ImmutableArray.read: array index out of bounds"
  )

scratch/main> run byteArrays

  ( Right
      ( [ 1
        , 258
        , 513
        , 131841
        , 66306
        , 50397699
        , 50462979
        , 4328719530
        , 730211746305
        , 72623846854952106
        , 12250920193496384001
        , 255
        ]
      , [ 1
        , 258
        , 513
        , 131841
        , 66306
        , 50397699
        , 50462979
        , 4328719530
        , 730211746305
        , 72623846854952106
        , 12250920193496384001
        , 32
        ]
      , 32
      , 0xs0304aa1122334455667788
      , 0xs0304aa1122334455667788010201020301020304aa
      , [ 1
        , 258
        , 513
        , 131841
        , 66306
        , 50397699
        , 50462979
        , 4328719530
        , 730211746305
        , 72623846854952106
        , 12250920193496384001
        , 32
        ]
      )
  , Left "MutableByteArray.read64le: array index out of bounds"
  , Left "MutableByteArray.read8: array index out of bounds"
  , Left "MutableByteArray.write16be: array index out of bounds"
  , Left "MutableByteArray.copyTo!: array index out of bounds"
  , Left "ImmutableByteArray_toBytes: array index out of bounds"
  , Left "MutableByteArray.freeze: array index out of bounds"
  )

scratch/main> run scoped

  Right (30, 7, 209, 255, 16843009, 4)

scratch/main> run tickets

  (5, true, 6, false, 6, true, 8)

scratch/main> run casTest

  49995000
```

Byte reads and writes at the edge of the array: each width and byte order at the last
in-bounds index, then one past it, then an index so large that adding the width wraps.
These take the generated checks' both branches (see "Data representations and the C
helpers" in internals.md).

``` unison
use Nat + - * / == < > <= >=

catchE : '{IO, Exception} a ->{IO} Either Text a
catchE c = handle c() with cases
  { a } -> Right a
  { Exception.raise f -> _ } -> match f with Failure _ msg _ -> Left msg

byteEdges : '{IO} ([Either Text Nat], [Either Text Nat], [Either Text ()], Either Text [Nat])
byteEdges = do
  b = IO.bytearrayOf 255 16
  f = MutableByteArray.freeze (IO.bytearrayOf 255 16) 0 16
  big = 18446744073709551615
  rd : (Nat, MutableByteArray {IO} -> Nat ->{IO, Exception} Nat) ->{IO} Either Text Nat
  rd = cases (i, k) -> catchE '(k b i)
  ri : (Nat, ImmutableByteArray -> Nat ->{Exception} Nat) ->{IO} Either Text Nat
  ri = cases (i, k) -> catchE '(k f i)
  wr : (Nat, MutableByteArray {IO} -> Nat -> Nat ->{IO, Exception} ()) ->{IO} Either Text ()
  wr = cases (i, k) -> catchE '(k b i 0)
  mut = List.map rd [ (15, MutableByteArray.read8), (16, MutableByteArray.read8),
      (14, MutableByteArray.read16be), (15, MutableByteArray.read16be),
      (14, MutableByteArray.read16le), (15, MutableByteArray.read16le),
      (13, MutableByteArray.read24be), (14, MutableByteArray.read24be),
      (13, MutableByteArray.read24le), (14, MutableByteArray.read24le),
      (12, MutableByteArray.read32be), (13, MutableByteArray.read32be),
      (12, MutableByteArray.read32le), (13, MutableByteArray.read32le),
      (11, MutableByteArray.read40be), (12, MutableByteArray.read40be),
      (11, MutableByteArray.read40le), (12, MutableByteArray.read40le),
      (8, MutableByteArray.read64be), (9, MutableByteArray.read64be),
      (8, MutableByteArray.read64le), (9, MutableByteArray.read64le),
      (big, MutableByteArray.read8), (big, MutableByteArray.read64le) ]
  imm = List.map ri [ (15, ImmutableByteArray.read8), (16, ImmutableByteArray.read8),
      (14, ImmutableByteArray.read16be), (15, ImmutableByteArray.read16le),
      (13, ImmutableByteArray.read24le), (14, ImmutableByteArray.read24be),
      (12, ImmutableByteArray.read32le), (13, ImmutableByteArray.read32be),
      (11, ImmutableByteArray.read40be), (12, ImmutableByteArray.read40le),
      (8, ImmutableByteArray.read64le), (9, ImmutableByteArray.read64be),
      (big, ImmutableByteArray.read16be) ]
  writes = List.map wr [ (15, MutableByteArray.write8), (16, MutableByteArray.write8),
      (14, MutableByteArray.write16be), (15, MutableByteArray.write16le),
      (12, MutableByteArray.write32le), (13, MutableByteArray.write32be),
      (8, MutableByteArray.write64be), (9, MutableByteArray.write64le),
      (big, MutableByteArray.write16be) ]
  -- the writes above zeroed bytes 8..15; a mixed-endian round trip over the zeros
  after = catchE do
    MutableByteArray.write32be b 8 2864434397
    MutableByteArray.write16le b 12 51966
    [ MutableByteArray.read64le b 8, MutableByteArray.read32le b 8, MutableByteArray.read16be b 12,
      MutableByteArray.read24be b 9, MutableByteArray.read40le b 7, MutableByteArray.read8 b 15 ]
  (mut, imm, writes, after)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + byteEdges : '{IO} ( [Either Text Nat],
                  [Either Text Nat],
                  [Either Text ()],
                  Either Text [Nat])
  + catchE    : '{IO, Exception} a ->{IO} Either Text a

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run byteEdges

  ( [ Right 255
    , Left "MutableByteArray.read8: array index out of bounds"
    , Right 65535
    , Left
        "MutableByteArray.read16be: array index out of bounds"
    , Right 65535
    , Left
        "MutableByteArray.read16le: array index out of bounds"
    , Right 16777215
    , Left
        "MutableByteArray.read24be: array index out of bounds"
    , Right 16777215
    , Left
        "MutableByteArray.read24le: array index out of bounds"
    , Right 4294967295
    , Left
        "MutableByteArray.read32be: array index out of bounds"
    , Right 4294967295
    , Left
        "MutableByteArray.read32le: array index out of bounds"
    , Right 1099511627775
    , Left
        "MutableByteArray.read40be: array index out of bounds"
    , Right 1099511627775
    , Left
        "MutableByteArray.read40le: array index out of bounds"
    , Right 18446744073709551615
    , Left
        "MutableByteArray.read64be: array index out of bounds"
    , Right 18446744073709551615
    , Left
        "MutableByteArray.read64le: array index out of bounds"
    , Left "MutableByteArray.read8: array index out of bounds"
    , Left
        "MutableByteArray.read64le: array index out of bounds"
    ]
  , [ Right 255
    , Left "ImmutableByteArray.read8: array index out of bounds"
    , Right 65535
    , Left
        "ImmutableByteArray.read16le: array index out of bounds"
    , Right 16777215
    , Left
        "ImmutableByteArray.read24be: array index out of bounds"
    , Right 4294967295
    , Left
        "ImmutableByteArray.read32be: array index out of bounds"
    , Right 1099511627775
    , Left
        "ImmutableByteArray.read40le: array index out of bounds"
    , Right 18446744073709551615
    , Left
        "ImmutableByteArray.read64be: array index out of bounds"
    , Left
        "ImmutableByteArray.read16be: array index out of bounds"
    ]
  , [ Right()
    , Left "MutableByteArray.write8: array index out of bounds"
    , Right()
    , Left
        "MutableByteArray.write16le: array index out of bounds"
    , Right()
    , Left
        "MutableByteArray.write32be: array index out of bounds"
    , Right()
    , Left
        "MutableByteArray.write64le: array index out of bounds"
    , Left
        "MutableByteArray.write16be: array index out of bounds"
    ]
  , Right
      [ 223195991686058
      , 3721182122
      , 65226
      , 12307677
      , 952622623487
      , 0
      ]
  )
```

## Universal.murmurHashUntyped

The hash walks the closures natively for numbers, characters, data constructors, text, bytes,
lists, arrays and byte arrays, and gives the interpreter's result bit for bit; a function, a
map or a link is left to the interpreter.

``` unison
use Nat + - * / == < > <= >=

structural type Shape = Dot | Line Nat Nat | Tri Nat Nat Nat

hashes : '{IO, Exception} [Nat]
hashes = do
  arr = IO.arrayOf 3 4
  MutableArray.write arr 0 (Universal.murmurHashUntyped "seed")
  frozen = MutableArray.freeze! arr
  b = IO.bytearrayOf 7 5
  fb = MutableByteArray.freeze! b
  [ Universal.murmurHashUntyped 0, Universal.murmurHashUntyped 42, Universal.murmurHashUntyped +42,
    Universal.murmurHashUntyped -42, Universal.murmurHashUntyped 1.5, Universal.murmurHashUntyped ?z,
    Universal.murmurHashUntyped Dot, Universal.murmurHashUntyped (Line 1 2), Universal.murmurHashUntyped (Tri 1 2 3),
    Universal.murmurHashUntyped (Some (Line 3 4)), Universal.murmurHashUntyped (None : Optional Nat),
    Universal.murmurHashUntyped "", Universal.murmurHashUntyped "héllo wörld ✓", Universal.murmurHashUntyped 0xs,
    Universal.murmurHashUntyped 0xsdeadbeef, Universal.murmurHashUntyped ([] : [Nat]), Universal.murmurHashUntyped [1, 2, 3],
    Universal.murmurHashUntyped [Some "a", None], Universal.murmurHashUntyped (1, "two", 3.0),
    Universal.murmurHashUntyped frozen, Universal.murmurHashUntyped fb,
    Universal.murmurHashUntyped (x -> x + 1) ]

hashLoop : Nat -> Nat
hashLoop n =
  go acc i = if i == n then acc else go (acc + Universal.murmurHashUntyped (Some (i, "x"))) (i + 1)
  go 0 0

> hashLoop 10000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Shape

  + hashes   : '{IO, Exception} [Nat]
  + hashLoop : Nat -> Nat

  Run `update` to apply these changes to your codebase.

    27 | > hashLoop 10000
           ⧩
           1345311805824347027
```

``` ucm
scratch/main> run hashes

  [ 6258706058885595179
  , 18339396765014686940
  , 18339396765014686940
  , 13508695160205268227
  , 14075555906852158245
  , 6853419648900567049
  , 14433742370595405518
  , 2413176120755755857
  , 6932268299374374561
  , 316041837696717613
  , 14433742370595405518
  , 13706802057387761162
  , 7199862675967212214
  , 6466841387522386278
  , 11995810339273904597
  , 1330906865450270697
  , 3347809220357464684
  , 5588272521198093502
  , 8249573216398372469
  , 9655031862619216264
  , 9566300282615928163
  , 12727184073911490092
  ]
```

## FFI pointers

Reads and writes through `Ptr` values: every element type, at the pointer and at an index,
with the sign extension, zero extension and truncation of the interpreter; a pointer stored
in and read back from a pointer to pointers; a pointer into a pinned byte array, whose writes
the array sees; and a cast pointer that reads the bytes of what another one wrote.

``` unison :hide
ptrInts : '{IO} [Int]
ptrInts = do
  i8 = FFI.Ptr.Int8.allocate 4
  FFI.Ptr.Int8.set i8 -5
  FFI.Ptr.Int8.setAt i8 1 +127
  FFI.Ptr.Int8.setAt i8 2 +200
  FFI.Ptr.Int8.setAt i8 3 -129
  i16 = FFI.Ptr.Int16.allocate 2
  FFI.Ptr.Int16.set i16 -300
  FFI.Ptr.Int16.setAt i16 1 +70000
  i32 = FFI.Ptr.Int32.allocate 2
  FFI.Ptr.Int32.set i32 -70000
  FFI.Ptr.Int32.setAt i32 1 +5000000000
  i64 = FFI.Ptr.Int.allocate 2
  FFI.Ptr.Int.set i64 -9000000000
  FFI.Ptr.Int.setAt i64 1 +42
  r = [ FFI.Ptr.Int8.get i8, FFI.Ptr.Int8.getAt i8 1, FFI.Ptr.Int8.getAt i8 2, FFI.Ptr.Int8.getAt i8 3,
        FFI.Ptr.Int16.get i16, FFI.Ptr.Int16.getAt i16 1,
        FFI.Ptr.Int32.get i32, FFI.Ptr.Int32.getAt i32 1,
        FFI.Ptr.Int.get i64, FFI.Ptr.Int.getAt i64 1 ]
  FFI.Ptr.free i8
  FFI.Ptr.free i16
  FFI.Ptr.free i32
  FFI.Ptr.free i64
  r

ptrNats : '{IO} [Nat]
ptrNats = do
  n8 = FFI.Ptr.Nat8.allocate 3
  FFI.Ptr.Nat8.set n8 255
  FFI.Ptr.Nat8.setAt n8 1 300
  FFI.Ptr.Nat8.setAt n8 2 18446744073709551615
  n16 = FFI.Ptr.Nat16.allocate 2
  FFI.Ptr.Nat16.set n16 65535
  FFI.Ptr.Nat16.setAt n16 1 70000
  n32 = FFI.Ptr.Nat32.allocate 2
  FFI.Ptr.Nat32.set n32 4294967295
  FFI.Ptr.Nat32.setAt n32 1 5000000000
  n64 = FFI.Ptr.Nat.allocate 2
  FFI.Ptr.Nat.set n64 18446744073709551615
  FFI.Ptr.Nat.setAt n64 1 42
  r = [ FFI.Ptr.Nat8.get n8, FFI.Ptr.Nat8.getAt n8 1, FFI.Ptr.Nat8.getAt n8 2,
        FFI.Ptr.Nat16.get n16, FFI.Ptr.Nat16.getAt n16 1,
        FFI.Ptr.Nat32.get n32, FFI.Ptr.Nat32.getAt n32 1,
        FFI.Ptr.Nat.get n64, FFI.Ptr.Nat.getAt n64 1 ]
  FFI.Ptr.free n8
  FFI.Ptr.free n16
  FFI.Ptr.free n32
  FFI.Ptr.free n64
  r

ptrFloats : '{IO} [Float]
ptrFloats = do
  f32 = FFI.Ptr.Float32.allocate 3
  FFI.Ptr.Float32.set f32 1.5
  FFI.Ptr.Float32.setAt f32 1 0.1
  FFI.Ptr.Float32.setAt f32 2 -3.0e38
  f64 = FFI.Ptr.Float.allocate 2
  FFI.Ptr.Float.set f64 2.25
  FFI.Ptr.Float.setAt f64 1 -0.1
  r = [ FFI.Ptr.Float32.get f32, FFI.Ptr.Float32.getAt f32 1, FFI.Ptr.Float32.getAt f32 2,
        FFI.Ptr.Float.get f64, FFI.Ptr.Float.getAt f64 1 ]
  FFI.Ptr.free f32
  FFI.Ptr.free f64
  r

ptrPtrs : '{IO} (Nat, Nat, Nat, Nat)
ptrPtrs = do
  a = FFI.Ptr.Nat.allocate 2
  FFI.Ptr.Nat.set a 7
  FFI.Ptr.Nat.setAt a 1 8
  b = FFI.Ptr.Nat.allocate 1
  FFI.Ptr.Nat.set b 9
  pp = FFI.Ptr.Ptr.allocate 2
  FFI.Ptr.Ptr.set pp a
  FFI.Ptr.Ptr.setAt pp 1 b
  a' = FFI.Ptr.Ptr.get pp
  b' = FFI.Ptr.Ptr.getAt pp 1
  r = (FFI.Ptr.Nat.get a', FFI.Ptr.Nat.getAt a' 1, FFI.Ptr.Nat.get b', FFI.Ptr.Nat.getAt (FFI.Ptr.Ptr.get pp) 1)
  FFI.Ptr.free a
  FFI.Ptr.free b
  FFI.Ptr.free pp
  r

-- a pointer into a pinned byte array, and a cast pointer: the two views
-- see the same bytes (little-endian on the machines this runs on)
ptrViews : '{IO, Exception} ([Nat], Nat, Nat, Int)
ptrViews = do
  arr = IO.pinnedByteArrayOf 0 8
  p = PinnedByteArray.contents arr
  FFI.Ptr.Nat8.setAt p 0 1
  FFI.Ptr.Nat8.setAt p 1 2
  p32 : Ptr Nat32
  p32 = FFI.Ptr.cast p
  FFI.Ptr.Nat32.setAt p32 1 16909060
  bytes = [ MutableByteArray.read8 (PinnedByteArray.cast arr) 0, MutableByteArray.read8 (PinnedByteArray.cast arr) 1,
            MutableByteArray.read8 (PinnedByteArray.cast arr) 4, MutableByteArray.read8 (PinnedByteArray.cast arr) 7 ]
  p16 : Ptr Nat16
  p16 = FFI.Ptr.cast p
  pi8 : Ptr Int8
  pi8 = FFI.Ptr.cast p
  (bytes, FFI.Ptr.Nat16.get p16, FFI.Ptr.Nat32.getAt p32 1, FFI.Ptr.Int8.getAt pi8 7)

-- the loop the benchmark runs, small
ptrLoop : Nat ->{IO} Nat
ptrLoop n =
  p = FFI.Ptr.Nat.allocate 4
  go acc i =
    if Nat.eq i n then acc
    else
      FFI.Ptr.Nat.setAt p 1 i
      FFI.Ptr.Nat.setAt p 2 (i + 1)
      go (acc + FFI.Ptr.Nat.getAt p 1 + FFI.Ptr.Nat.getAt p 2) (i + 1)
  r = go 0 0
  FFI.Ptr.free p
  r

catchP : '{IO, Exception} a ->{IO} Either Text a
catchP c = handle c() with cases
  { a } -> Right a
  { Exception.raise f -> _ } -> match f with Failure _ msg _ -> Left msg

pointers : '{IO} ([Int], [Nat], [Float], (Nat, Nat, Nat, Nat), Either Text ([Nat], Nat, Nat, Int), Nat)
pointers = do
  (ptrInts(), ptrNats(), ptrFloats(), ptrPtrs(), catchP ptrViews, ptrLoop 1000)
```

``` ucm
scratch/main> run pointers

  ( [ -5
    , +127
    , -56
    , +127
    , -300
    , +4464
    , -70000
    , +705032704
    , -9000000000
    , +42
    ]
  , [ 255
    , 44
    , 255
    , 65535
    , 4464
    , 4294967295
    , 705032704
    , 18446744073709551615
    , 42
    ]
  , [ 1.5
    , 0.10000000149011612
    , -3.0000000054977558e38
    , 2.25
    , -0.1
    ]
  , (7, 8, 9, 8)
  , Right ([1, 2, 4, 1], 513, 16909060, +1)
  , 1000000
  )
```

## Universal equality and ordering on every kind of value

`Universal.==`, `<`, `<=`, `>`, `>=` and `compare` are native for every value: two numbers of
the same type inline, everything else through a walker over the closures (`unison_jit_ueq`,
`unison_jit_ucmp`) that gives the interpreter's answer, including its quirks: a `Nat` and an
`Int` of the same value are equal, ordering compares constructor indices only while equality
compares the type too, `-0.0` and `0.0` are equal under ordering but not under equality (the
bits differ). Functions of different definitions and maps are still left to the interpreter.

``` unison
use Nat + - * / == < > <= >=

structural type Shape = Dot | Line Nat Nat | Tri Nat Nat Nat

cmpAll : a -> a -> (Boolean, Boolean, Boolean, Boolean, Boolean, Int)
cmpAll x y = (x Universal.== y, x Universal.< y, x Universal.<= y, x Universal.> y, x Universal.>= y, Universal.compare x y)

-- an insertion sort by Universal.<=, so that the comparisons are in compiled code
insertSorted : a -> [a] -> [a]
insertSorted x = cases
  [] -> [x]
  y +: ys -> if x Universal.<= y then x +: (y +: ys) else y +: insertSorted x ys

sortU : [a] -> [a]
sortU xs =
  go acc = cases
    [] -> acc
    x +: rest -> go (insertSorted x acc) rest
  go [] xs

addOne : Nat -> Nat
addOne x = x + 1

addN : Nat -> Nat -> Nat
addN a b = a + b

-- how many of the pairs (i, i * 7 % 13) order below (n / 2, 6), n times
countBelow : Nat -> Nat
countBelow n =
  go acc i = if i == n then acc else
    go (if (i, Nat.mod (i * 7) 13) Universal.< (n / 2, 6) then acc + 1 else acc) (i + 1)
  go 0 0

> cmpAll (1, "a") (1, "b")
> cmpAll (1, "b") (1, "a")
> cmpAll (2, "a") (2, "a")
> cmpAll (Line 1 2) (Line 1 3)
> cmpAll (Tri 1 2 3) (Tri 1 2 3)
> cmpAll Dot (Line 0 0)
> cmpAll (Some 1) None
> cmpAll (Some (Some 1)) (Some (Some 2))
> cmpAll [1, 2, 3] [1, 2]
> cmpAll [(1, "x"), (2, "y")] [(1, "x"), (2, "z")]
> cmpAll ([] : [Nat]) []
> cmpAll [Some "a", None] [Some "a", None]
> cmpAll (Any +1) (Any 1)
> cmpAll (Any -1) (Any 1)
> cmpAll (Any ?a) (Any 97)
> cmpAll 1.5 2.5
> cmpAll -1.0 -2.0
> cmpAll -1.0 2.0
> cmpAll 0.0 -0.0
> cmpAll 0.0 -1.0
> cmpAll -1.0e10 0.0
> cmpAll (0.0 Float./ 0.0) (0.0 Float./ 0.0)
> cmpAll ?a ?b
> cmpAll true false
> cmpAll false false
> cmpAll +3 -3
> cmpAll 18446744073709551615 1
> cmpAll "abc" "abd"
> cmpAll 0xs01 0xs0100
> cmpAll (1, 0xs01, "a", ?c, 2.0) (1, 0xs01, "a", ?c, 2.0)
> cmpAll addOne addOne
> cmpAll (addN 1) (addN 2)
> cmpAll (addN 2) (addN 2)
> sortU [(3, "c"), (1, "b"), (2, "a"), (1, "a"), (3, "a")]
> sortU [Some 3, None, Some 1, None, Some 2]
> sortU [[2, 1], [1, 2, 3], [], [1, 2]]
> sortU [-1.5, 2.0, -0.0, 0.0, 1e10, -1e10]
> countBelow 100000

cmpArrays : '{IO, Exception} (Boolean, Int, Boolean, Int, Boolean, Int)
cmpArrays = do
  a = IO.arrayOf 1 3
  b = IO.arrayOf 1 3
  MutableArray.write b 2 5
  fa = MutableArray.freeze! a
  fb = MutableArray.freeze! b
  ba = IO.bytearrayOf 7 4
  bb = IO.bytearrayOf 7 5
  fba = MutableByteArray.freeze! ba
  fbb = MutableByteArray.freeze! bb
  (fa Universal.== fb, Universal.compare fa fb, fa Universal.== fa, Universal.compare fb fa, fba Universal.== fbb, Universal.compare fba fbb)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Shape

  + addN         : Nat -> Nat -> Nat
  + addOne       : Nat -> Nat
  + cmpAll       : a
                   -> a
                   -> ( Boolean,
                     Boolean,
                     Boolean,
                     Boolean,
                     Boolean,
                     Int)
  + cmpArrays    : '{IO, Exception} ( Boolean,
                     Int,
                     Boolean,
                     Int,
                     Boolean,
                     Int)
  + countBelow   : Nat -> Nat
  + insertSorted : a -> [a] -> [a]
  + sortU        : [a] -> [a]

  Run `update` to apply these changes to your codebase.

    34 | > cmpAll (1, "a") (1, "b")
           ⧩
           (false, true, true, false, false, -1)

    35 | > cmpAll (1, "b") (1, "a")
           ⧩
           (false, false, false, true, true, +1)

    36 | > cmpAll (2, "a") (2, "a")
           ⧩
           (true, false, true, false, true, +0)

    37 | > cmpAll (Line 1 2) (Line 1 3)
           ⧩
           (false, true, true, false, false, -1)

    38 | > cmpAll (Tri 1 2 3) (Tri 1 2 3)
           ⧩
           (true, false, true, false, true, +0)

    39 | > cmpAll Dot (Line 0 0)
           ⧩
           (false, true, true, false, false, -1)

    40 | > cmpAll (Some 1) None
           ⧩
           (false, true, true, false, false, -1)

    41 | > cmpAll (Some (Some 1)) (Some (Some 2))
           ⧩
           (false, true, true, false, false, -1)

    42 | > cmpAll [1, 2, 3] [1, 2]
           ⧩
           (false, false, false, true, true, +1)

    43 | > cmpAll [(1, "x"), (2, "y")] [(1, "x"), (2, "z")]
           ⧩
           (false, true, true, false, false, -1)

    44 | > cmpAll ([] : [Nat]) []
           ⧩
           (true, false, true, false, true, +0)

    45 | > cmpAll [Some "a", None] [Some "a", None]
           ⧩
           (true, false, true, false, true, +0)

    46 | > cmpAll (Any +1) (Any 1)
           ⧩
           (true, false, true, false, true, +0)

    47 | > cmpAll (Any -1) (Any 1)
           ⧩
           (false, true, true, false, false, -1)

    48 | > cmpAll (Any ?a) (Any 97)
           ⧩
           (false, true, true, false, false, -1)

    49 | > cmpAll 1.5 2.5
           ⧩
           (false, true, true, false, false, -1)

    50 | > cmpAll -1.0 -2.0
           ⧩
           (false, false, false, true, true, +1)

    51 | > cmpAll -1.0 2.0
           ⧩
           (false, true, true, false, false, -1)

    52 | > cmpAll 0.0 -0.0
           ⧩
           (false, false, true, false, true, +0)

    53 | > cmpAll 0.0 -1.0
           ⧩
           (false, false, false, true, true, +1)

    54 | > cmpAll -1.0e10 0.0
           ⧩
           (false, true, true, false, false, -1)

    55 | > cmpAll (0.0 Float./ 0.0) (0.0 Float./ 0.0)
           ⧩
           (true, false, true, false, true, +0)

    56 | > cmpAll ?a ?b
           ⧩
           (false, true, true, false, false, -1)

    57 | > cmpAll true false
           ⧩
           (false, false, false, true, true, +1)

    58 | > cmpAll false false
           ⧩
           (true, false, true, false, true, +0)

    59 | > cmpAll +3 -3
           ⧩
           (false, false, false, true, true, +1)

    60 | > cmpAll 18446744073709551615 1
           ⧩
           (false, false, false, true, true, +1)

    61 | > cmpAll "abc" "abd"
           ⧩
           (false, true, true, false, false, -1)

    62 | > cmpAll 0xs01 0xs0100
           ⧩
           (false, true, true, false, false, -1)

    63 | > cmpAll (1, 0xs01, "a", ?c, 2.0) (1, 0xs01, "a", ?c, 2.0)
           ⧩
           (true, false, true, false, true, +0)

    64 | > cmpAll addOne addOne
           ⧩
           (true, false, true, false, true, +0)

    65 | > cmpAll (addN 1) (addN 2)
           ⧩
           (false, true, true, false, false, -1)

    66 | > cmpAll (addN 2) (addN 2)
           ⧩
           (true, false, true, false, true, +0)

    67 | > sortU [(3, "c"), (1, "b"), (2, "a"), (1, "a"), (3, "a")]
           ⧩
           [(1, "a"), (1, "b"), (2, "a"), (3, "a"), (3, "c")]

    68 | > sortU [Some 3, None, Some 1, None, Some 2]
           ⧩
           [Some 1, Some 2, Some 3, None, None]

    69 | > sortU [[2, 1], [1, 2, 3], [], [1, 2]]
           ⧩
           [[], [1, 2], [1, 2, 3], [2, 1]]

    70 | > sortU [-1.5, 2.0, -0.0, 0.0, 1e10, -1e10]
           ⧩
           [-1.0e10, -1.5, 0.0, -0.0, 2.0, 1.0e10]

    71 | > countBelow 100000
           ⧩
           50001
```

``` ucm
scratch/main> run cmpArrays

  (false, -1, true, +1, false, -1)
```
