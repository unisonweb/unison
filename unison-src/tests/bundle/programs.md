A smoke test of bundles and unisonrt, run by `scripts/bundle-check.sh`. It
compiles these programs, bundles them with ucm's `bundle` command (the script
also uses `ucm bundle` and `unisonrt bundle`), runs them with the JIT off and
on, and compares what they print with `expected.txt`.

The programs exercise what a bundle has to get right (its arguments, `+RTS`
included, printing values on one line, `Debug.trace`, and each way a program
fails) and, in `checks`, a little of everything the runtime does, borrowed from
the JIT's tests (unison-src/transcripts/idempotent/jit-tests.md) at sizes that
run in a moment. Run with the JIT on, the loops get hot enough to be compiled,
so a minified unisonrt's JIT is exercised as well as its interpreter.

It runs in a project of its own, so that it can run on a codebase other tests
use too (CI runs it on the JIT tests' codebase).

```ucm :hide
scratch/main> project.create-empty bundle-check
bundle-check/main> builtins.mergeio lib.builtins
```

```unison :hide
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

printLine : Text ->{IO, Exception} ()
printLine t = match putBytes.impl (stdHandle StdOut) (Text.toUtf8 (t ++ "\n")) with
  Right () -> ()
  Left f -> Exception.raise f

-- a value as unisonrt prints it, after a label
show : Text -> a ->{IO, Exception} ()
show label x = printLine match Debug.toText x with
  Some (Left t) -> label ++ ": " ++ t
  Some (Right t) -> label ++ ": " ++ t
  None -> label ++ ": ?"

args : '{IO, Exception} [Text]
args = do match getArgs.impl () with
  Right as -> as
  Left f -> Exception.raise f

joinArgs : [Text] -> Text
joinArgs = cases
  [] -> ""
  a +: rest -> "[" ++ a ++ "]" ++ joinArgs rest

hello : '{IO, Exception} ()
hello = do
  printLine "hello"
  printLine ("args: " ++ joinArgs (args()))

type Shape = Circle Nat | Rect Nat Nat | Tri Nat Nat Nat | Dot
structural type Pt = Pt Int Float Text

values : '{IO, Exception} ()
values = do
  _ = Debug.trace "shapes" [Circle 1, Rect 2 3]
  _ = Debug.trace "mixed" (Pt -3 1.5 "a \"q\"\n", ?x, 0xs01ff, Some (Some true), (), [None, Some 4])
  _ = Debug.trace "function" (x -> x Nat.+ 1)
  printLine match Debug.toText (Left (Rect 5 6)) with
    Some (Left t) -> t
    Some (Right t) -> t
    None -> "none"

crash : '{IO, Exception} ()
crash = do
  printLine "about to fail"
  bug (Some (Rect 1 2), "oops")

raises : '{IO, Exception} ()
raises = do
  _ = Exception.raise (Failure (typeLink Shape) "my failure" (Any (Circle 3)))
  ()

divZero : '{IO, Exception} ()
divZero = do printLine (Nat.toText (10 / Nat.drop 3 3))
```

```ucm :hide
bundle-check/main> update
```

From the JIT's tests: loops, recursion, data, abilities, lists, text and bytes.

```unison :hide
use Nat + - * / == < > <=

sumTo : Nat -> Nat
sumTo n =
  go acc i = if i > n then acc else go (acc + i) (i + 1)
  go 0 0

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

isEven : Nat -> Boolean
isEven n = if n == 0 then true else isOdd (n - 1)

isOdd : Nat -> Boolean
isOdd n = if n == 0 then false else isEven (n - 1)

fib : Nat -> Nat
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

depth : Nat -> Nat
depth n = if n == 0 then 0 else 1 + depth (n - 1)

ackermann : Nat -> Nat -> Nat
ackermann m n =
  if m == 0 then n + 1
  else if n == 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

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

Tree.build : Nat -> Tree
Tree.build n =
  go t i =
    if i == n then t
    else go (Tree.insert (Nat.mod (i * 7919) 10007) i t) (i + 1)
  go Tree.Leaf 0

area : Shape -> Nat
area = cases
  Circle r -> 3 * r * r
  Rect w h -> w * h
  Tri a b c -> a + b + c
  Dot -> 0

shapes : Nat -> Nat
shapes n =
  go acc i =
    if i == n then acc
    else
      s = match Nat.mod i 4 with
        0 -> Circle i
        1 -> Rect i 2
        2 -> Tri i i i
        _ -> Dot
      go (acc + area s) (i + 1)
  go 0 0

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

runChoose : '{Choose, g} a ->{g} [a]
runChoose thunk =
  h : Request {Choose} a -> [a]
  h = cases
    { a } -> [a]
    { Choose.choose -> k } -> (handle k true with h) List.++ (handle k false with h)
  handle !thunk with h

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

squares : Nat -> [Nat]
squares n =
  go acc i = if i == n then acc else go (acc :+ (i * i)) (i + 1)
  go [] 0

words : Nat -> Text
words n =
  go acc i = if i == n then acc else go (acc ++ Nat.toText i ++ " ") (i + 1)
  go "" 0

checks : '{IO, Exception} ()
checks = do
  show "sumTo" (sumTo 10000)
  show "collatz" (collatz 27)
  show "intLoop" (intLoop +10)
  show "floatLoop" (floatLoop 1000)
  show "parity" (isEven 1001, isOdd 1001)
  show "bits" (Nat.shiftLeft 1 40, Nat.and 255 1023, Nat.xor 5 3, Nat.pow 3 20, 0 - 1)
  show "fib" (fib 18)
  show "depth" (depth 10000)
  show "ackermann" (ackermann 2 3)
  show "cons" (Cons.foldLeft (+) 0 (Cons.map (x -> x * 2) (Cons.range 0 1000)))
  t = Tree.build 500
  show "tree" (Tree.lookup 7919 t, Tree.lookup 10008 t)
  show "shapes" (shapes 2000)
  show "counter" (runCounter 0 '(sumNext 100))
  show "abort" (runAbort '(findFirst 144 0), runAbort '(findFirst 145 0))
  show "choose" (runChoose '(bits 3))
  show "list" (List.size (squares 1000), List.at 999 (squares 1000))
  show "text" (Text.size (words 300), Text.take 20 (words 300))
  show "bytes" (Bytes.size (Text.toUtf8 (words 300)), Bytes.take 4 (Text.toUtf8 "héllo"))
```

```ucm :hide
bundle-check/main> update
bundle-check/main> compile hello hello
bundle-check/main> compile values values
bundle-check/main> compile checks checks
bundle-check/main> compile crash crash
bundle-check/main> compile raises raises
bundle-check/main> compile divZero divZero
bundle-check/main> bundle hello hello-ucm
bundle-check/main> bundle checks checks-ucm
```
