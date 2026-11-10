# The non-moving collector's write barrier

Two large mutable arrays, filled with lists at the start and old after the first major GC, whose elements are swapped between them: each round exchanges one element of each, writes a fresh list into a third array so that promotions keep reusing freed memory, and allocates garbage to keep the collector busy; at the end the sizes of every element of both arrays are summed, which a slot pointing into freed memory corrupts or crashes. Under the non-moving collector (`+RTS -xn`) the old generation is marked concurrently with the program from a snapshot, and a value moved into a slot the marker has already visited, out of one it has not, is reachable at the end of the mark only through the pointer the write overwrote; the barrier native code generates under that collector hands the marker that pointer (JIT/design.md, "The write barrier"). Without it the value is freed under the program. The swaps go both ways because the marker visits the two arrays in a fixed order, and a move in the other direction loses nothing.

Run against the `jit` testing codebase (`scripts/testing-codebase.sh jit`) on the optimized build, `UNISON_JIT=eager`, with `+RTS -xn -A4m -F1.1 -RTS` (a small nursery and a major GC after every 10% of growth, so that about fifty marks run during the loop; about 20 s), which must print 349997164999172; then again with `UNISON_JIT_DEBUG=1 UNISON_JIT_DISABLE=nmbarrier` (the disable variable is read only under the debug variable), the control, which must fail: without the barrier the marker aborts within seconds with "mark_closure: unimplemented/strange closure type", or the checksum comes out wrong (three runs out of three, 2026-11-06; with 500,000-element arrays the checksum is wrong every time instead). See JIT/development.md, "Testing".

``` unison
use Nat + * - == mod

mkList : Nat -> [Nat]
mkList n = if n == 0 then [] else n +: mkList (n - 1)

nmStress : Nat -> Nat ->{IO, Exception} Nat
nmStress rounds size =
  a = IO.Raw.arrayOf [0] size
  b = IO.Raw.arrayOf [1] size
  c = IO.Raw.arrayOf [2] size
  fill i =
    if i == size then ()
    else
      mutable.Array.Raw.write a i (mkList (mod i 7 + 1))
      mutable.Array.Raw.write b i (mkList (mod i 5 + 1))
      fill (i + 1)
  fill 0
  go i acc =
    if i == rounds then acc
    else
      j = mod (i * 7919) size
      k = mod (i * 104729 + 13) size
      x = mutable.Array.Raw.read a j
      y = mutable.Array.Raw.read b k
      mutable.Array.Raw.write b k x
      mutable.Array.Raw.write a j y
      mutable.Array.Raw.write c (mod i size) (mkList (mod i 13 + 1))
      garbage = List.size (List.map (n -> n * 2) (mkList (mod i 97)))
      go (i + 1) (acc + List.size x + List.size y + garbage)
  total = go 0 0
  check i acc =
    if i == size then acc
    else check (i + 1) (acc + List.size (mutable.Array.Raw.read a i) + List.size (mutable.Array.Raw.read b i))
  total + check 0 0 * 1000000000

nmStressTest : '{IO, Exception} Nat
nmStressTest = do nmStress 3000000 50000
```

``` ucm
jit-tests/main> add
jit-tests/main> run nmStressTest
```
