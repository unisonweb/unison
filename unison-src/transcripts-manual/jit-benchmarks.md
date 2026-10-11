# JIT benchmarks

Run this against the `jit_codebase` codebase, which has the `jit-tests` project with the base
library and the timing helpers (`printTime`, `repeat`) from `@pchiusano/misc-benchmarks` in it.
Use an optimized build:

```
stack build && stack exec unison -- -C jit_codebase transcript.fork unison-src/transcripts-manual/jit-benchmarks.md
```

The timings are printed to the console as the transcript runs.

These measure what the JIT compiles, without lists or functions that have Haskell replacements.

``` unison
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

Tree.lookupAll : Nat -> Tree -> Nat
Tree.lookupAll n t =
  go found i =
    if i == n then found
    else match Tree.lookup (Nat.mod (i * 7919) 10007) t with
      None -> go found (i + 1)
      Some _ -> go (found + 1) (i + 1)
  go 0 0

sumTo : Nat -> Nat
sumTo n =
  go acc i = if i > n then acc else go (acc + i) (i + 1)
  go 0 0

fib : Nat -> Nat
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

applyN : Nat -> (a ->{g} a) -> a ->{g} a
applyN n f a = if n == 0 then a else applyN (n - 1) f (f a)

refLoop : Nat ->{IO} Nat
refLoop n =
  r = IO.ref 0
  go i =
    if i == n then Ref.read r
    else
      Ref.write r (Ref.read r + i)
      go (i + 1)
  go 0

-- A hot loop that calls small functions from other definitions: what
-- batching and direct calls are for.
collatzStep : Nat -> Nat
collatzStep n = if Nat.mod n 2 == 0 then n / 2 else 3 * n + 1

collatzSteps : Nat -> Nat
collatzSteps n =
  go steps x = if x == 1 then steps else go (steps + 1) (collatzStep x)
  go 0 n

collatzTotal : Nat -> Nat
collatzTotal n =
  go acc i = if i > n then acc else go (acc + collatzSteps i) (i + 1)
  go 0 1

-- Text: every operation here is a call-out until native code can work on
-- the rope directly.
textAppend : Nat -> Text
textAppend n =
  go acc i = if i == n then acc else go (acc Text.++ "hi") (i + 1)
  go "" 0

textDrain : Nat -> Text -> Text
textDrain n t =
  go rem i = if i == n then rem else go (Text.drop 1 rem) (i + 1)
  go t 0

-- Bytes: the same rope as Text, with the same native operations
bytesAppend : Nat -> Bytes -> Bytes
bytesAppend n piece =
  go acc i = if i == n then acc else go (acc Bytes.++ piece) (i + 1)
  go Bytes.empty 0

bytesDrain : Nat -> Bytes -> Bytes
bytesDrain n b =
  go rem i = if i == n then rem else go (Bytes.drop 1 rem) (i + 1)
  go b 0

-- the characters one at a time, with uncons
textWalk : Text -> Nat
textWalk t =
  go acc t = match Text.uncons t with
    None -> acc
    Some (c, rest) -> go (acc + 1) rest
  go 0 t

-- numbers to text and back
natRound : Nat -> Nat
natRound n =
  go acc i = if i == n then acc else
    go (acc + Optional.getOrElse 0 (Nat.fromText (Nat.toText i))) (i + 1)
  go 0 0

-- 8-byte numbers off the front
bytesDecode : Bytes -> Nat
bytesDecode b =
  go acc b = match Bytes.decodeNat64be b with
    None -> acc
    Some (n, rest) -> go (acc + n) rest
  go 0 b

-- every byte, by position
bytesSum : Bytes -> Nat
bytesSum b =
  go acc i = match Bytes.at i b with
    None -> acc
    Some x -> go (acc + x) (i + 1)
  go 0 0

floatWalk : Nat -> Float
floatWalk n =
  go acc i =
    if i == n then acc
    else
      x = Nat.toFloat i
      y = Float.sqrt x Float.+ Float.sin x Float.* Float.cos x
      z = Float.max y 1.0 Float.+ Int.toFloat (##Float.truncate y) Float.+ Int.toFloat (##Float.round (Float.abs y))
      go (acc Float.+ z Float./ 3.0) (i + 1)
  go 0.0 0

byteArrayLoop : Nat ->{IO, Exception} Nat
byteArrayLoop n =
  b = IO.Raw.byteArrayOf 0 64
  go acc i =
    if i == n then acc
    else
      mutable.ByteArray.Raw.write64le b 8 i
      mutable.ByteArray.Raw.write32be b 0 i
      mutable.ByteArray.Raw.write16le b 20 i
      mutable.ByteArray.Raw.write8 b 30 i
      go (acc + mutable.ByteArray.Raw.read64le b 8 + mutable.ByteArray.Raw.read32be b 0 + mutable.ByteArray.Raw.read24be b 1 + mutable.ByteArray.Raw.read40le b 16 + mutable.ByteArray.Raw.read16le b 20 + mutable.ByteArray.Raw.read8 b 30) (i + 1)
  go 0 0

-- the same through FFI pointers
ptrLoop : Nat ->{IO} Nat
ptrLoop n =
  p = FFI.Ptr.Nat.allocate 4
  p32 : Ptr Nat32
  p32 = FFI.Ptr.cast p
  p16 : Ptr Nat16
  p16 = FFI.Ptr.cast p
  p8 : Ptr Nat8
  p8 = FFI.Ptr.cast p
  go acc i =
    if i == n then acc
    else
      FFI.Ptr.Nat.setAt p 1 i
      FFI.Ptr.Nat32.set p32 i
      FFI.Ptr.Nat16.setAt p16 10 i
      FFI.Ptr.Nat8.setAt p8 30 i
      go (acc + FFI.Ptr.Nat.getAt p 1 + FFI.Ptr.Nat32.get p32 + FFI.Ptr.Nat8.getAt p8 1 + FFI.Ptr.Nat.getAt p 2 + FFI.Ptr.Nat16.getAt p16 10 + FFI.Ptr.Nat8.getAt p8 30) (i + 1)
  r = go 0 0
  FFI.Ptr.free p
  r

arrayFillSum : Nat ->{IO, Exception} Nat
arrayFillSum n =
  arr = IO.Raw.arrayOf 0 n
  fill i = if i == n then () else
    mutable.Array.Raw.write arr i (i * 3)
    fill (i + 1)
  sum acc i = if i == n then acc else sum (acc + mutable.Array.Raw.read arr i) (i + 1)
  fill 0
  frozen = mutable.Array.Raw.freeze! arr
  sumI acc i = if i == n then acc else sumI (acc + data.Array.Raw.read frozen i) (i + 1)
  sum 0 0 + sumI 0 0

casLoop : Nat ->{IO} Nat
casLoop n =
  r = IO.ref 0
  go i =
    if i == n then Ref.read r
    else
      t = IO.ref.readForCas r
      if IO.ref.cas r t (IO.ref.Ticket.read t + i) then go (i + 1) else go i
  go 0

hashLoop : Nat -> Nat
hashLoop n =
  go : Nat -> Nat -> Nat
  go acc i = if i == n then acc else go (acc Nat.+ ##Universal.murmurHashUntyped (Some (i, "x"))) (i Nat.+ 1)
  go 0 0

-- A callee that is first reached after its caller is hot: the first 20000
-- iterations take the other branch (the batch forms on the compile thread
-- about a millisecond after the trigger, a thousand or so iterations, so
-- the phase has to be long). A batch rule that judged a callee by its own
-- count, zero here, compiled the caller without it; the callee then exited
-- to the interpreter until its own count made it hot, was compiled alone,
-- and was called through its cell from the caller's native code for the
-- rest of the run: 3x slower. The batch rule weighs the edge by the
-- caller's count (one site, once per call) and takes the callee along, so
-- the call is direct and LLVM inlines it.
oneInThree : Nat -> Nat
oneInThree x = x * 7 + 3

-- The call is not in tail position, which keeps Unison's own inliner
-- from inlining the callee (ANF.Optimize: a body with bindings is only
-- inlined at a tail call).
mostlyInline : Nat -> Nat
mostlyInline x = if x < 20000 then x + 1 else oneInThree x + 1

branchCalls : Nat -> Nat
branchCalls n =
  go acc i = if i == n then acc else go (acc + mostlyInline i) (i + 1)
  go 0 0

-- Affine handlers: a request is a call of the handler found in the
-- environment, and the handler's case returns to the requester.
structural ability Counter where
  next : Nat

runCounter : Nat -> '{Counter, g} a ->{g} a
runCounter start thunk =
  h : Nat -> Request {Counter} a -> a
  h n = cases
    { a } -> a
    { Counter.next -> k } -> handle k n with h (n + 1)
  handle !thunk with h start

sumNext : Nat -> Nat ->{Counter} Nat
sumNext acc n = if n == 0 then acc else sumNext (acc + Counter.next) (n - 1)

structural ability Store a where
  get : a
  put : a -> ()

runStore : s -> '{Store s, g} a ->{g} a
runStore s thunk =
  h : s -> Request {Store s} a -> a
  h s = cases
    { a } -> a
    { Store.get -> k } -> handle k s with h s
    { Store.put s' -> k } -> handle k () with h s'
  handle !thunk with h s

loopStore : Nat ->{Store Nat} Nat
loopStore n = if n == 0 then Store.get else
  Store.put (Store.get + 1)
  loopStore (n - 1)

-- the handler itself is installed each time around: `handle` is the interpreter's
handleLoop : Nat -> Nat -> Nat
handleLoop acc n = if n == 0 then acc else
  r = runStore 1 '(Store.get + Store.get)
  handleLoop (acc + r) (n - 1)

-- a handler whose put case performs a request of an outer ability
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

jitSuite : '{IO, Exception} ()
jitSuite = do
  printTime "Sum 0 to 1 million" 1 (n -> repeat n do sumTo 1000000)
  printTime "fib 20" 1 (n -> repeat n do fib 20)
  printTime "Cons list: map with a lambda (1000 elements)" 1 let
    c = Cons.range 0 1000
    n -> repeat n do Cons.map (x -> x + 1) c
  printTime "Cons list: foldLeft with a lambda (1000 elements)" 1 let
    c = Cons.range 0 1000
    n -> repeat n do Cons.foldLeft (acc x -> acc + x) 0 c
  printTime "Binary tree: 1000 inserts" 1 (n -> repeat n do Tree.build 1000)
  printTime "Binary tree: 1000 lookups" 1 let
    t = Tree.build 1000
    n -> repeat n do Tree.lookupAll 1000 t
  printTime "Apply a function argument 10000 times" 1 (n -> repeat n do applyN 10000 (x -> x + 3) 0)
  printTime "Mutate a Ref 10000 times" 1 (n -> repeat n do refLoop 10000)
  printTime "Calls across definitions: Collatz steps for 1 to 1000" 1 (n -> repeat n do collatzTotal 1000)
  printTime "Calls across definitions: callee first reached once the caller is hot, 100000 iterations" 1 (n -> repeat n do branchCalls 100000)
  printTime "Text: append \"hi\" 10000 times" 1 (n -> repeat n do textAppend 10000)
  printTime "Text: drop 1, 100000 times" 1 let
    t = Text.repeat 100000 "a"
    n -> repeat n do textDrain 100000 t
  printTime "Bytes: append 2 bytes 10000 times" 1 let
    hi = Bytes.fromList [104, 105]
    n -> repeat n do bytesAppend 10000 hi
  printTime "Bytes: drop 1, 100000 times" 1 let
    b = bytesAppend 50000 (Bytes.fromList [104, 105])
    n -> repeat n do bytesDrain 100000 b
  printTime "Bytes: at, 100000 times" 1 let
    b = bytesAppend 50000 (Bytes.fromList [104, 105])
    n -> repeat n do bytesSum b
  printTime "Text: uncons walk over 100000 characters" 1 let
    t = Text.repeat 100000 "a"
    n -> repeat n do textWalk t
  printTime "Nat.toText and Nat.fromText, 10000 times" 1 (n -> repeat n do natRound 10000)
  printTime "Float: sqrt, sin, cos, arithmetic and conversions, 100000 times" 1 (n -> repeat n do floatWalk 100000)
  printTime "MutableByteArray: 4 writes and 6 reads, both byte orders, 100000 times" 1 (n -> repeat n do byteArrayLoop 100000)
  printTime "FFI.Ptr: 4 writes and 6 reads, 100000 times" 1 (n -> repeat n do ptrLoop 100000)
  printTime "MutableArray: fill, freeze and sum 10000 elements" 1 (n -> repeat n do arrayFillSum 10000)
  printTime "Ref.cas loop, 10000 times" 1 (n -> repeat n do casLoop 10000)
  printTime "murmurHashUntyped of Some (i, \"x\"), 10000 times" 1 (n -> repeat n do hashLoop 10000)
  printTime "Counter: 100000 requests" 1 (n -> repeat n do runCounter 0 '(sumNext 0 100000))
  printTime "Store: get and put, 100000 iterations" 1 (n -> repeat n do runStore 0 '(loopStore 100000))
  printTime "handle installed per iteration, 10000 iterations" 1 (n -> repeat n do handleLoop 0 10000)
  printTime "Store whose put requests an outer Counter, 100000 iterations" 1 (n -> repeat n do runCounter 0 '(countingStore 0 '(loopStore 100000)))
  printTime "Bytes: decodeNat64be walk over 80000 bytes" 1 let
    b = bytesAppend 40000 (Bytes.fromList [104, 105])
    n -> repeat n do bytesDecode b
```

``` ucm
jit-tests/main> run jitSuite
```
