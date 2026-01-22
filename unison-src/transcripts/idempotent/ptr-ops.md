``` ucm :hide
scratch/ptr> builtins.mergeio
```

This contains a sequence of tests that exercise the pointer operations.

``` unison
floatTest = do
  p = Float.allocate 1
  Float.set p 1.0
  x = Float.get p
  Float.setAt p 0 2.0
  y = Float.getAt p 0
  Ptr.free p
  (x == 1.0, y == 2.0)

float32Test = do
  p = Float32.allocate 1
  Float32.set p 1.0
  x = Float32.get p
  Float32.setAt p 0 2.0
  y = Float32.getAt p 0
  Ptr.free p
  (x == 1.0, y == 2.0)

int8Test = do
  p = Int8.allocate 1
  Int8.set p +1
  x = Int8.get p
  Int8.setAt p 0 +2
  y = Int8.getAt p 0
  Ptr.free p
  (x == +1, y == +2)

int16Test = do
  p = Int16.allocate 1
  Int16.set p +1
  x = Int16.get p
  Int16.setAt p 0 +2
  y = Int16.getAt p 0
  Ptr.free p
  (x == +1, y == +2)

int32Test = do
  p = Int32.allocate 1
  Int32.set p +1
  x = Int32.get p
  Int32.setAt p 0 +2
  y = Int32.getAt p 0
  Ptr.free p
  (x == +1, y == +2)

int64Test = do
  p = Int.allocate 1
  Int.set p +1
  x = Int.get p
  Int.setAt p 0 +2
  y = Int.getAt p 0
  Ptr.free p
  (x == +1, y == +2)

nat8Test = do
  p = Nat8.allocate 1
  Nat8.set p 1
  x = Nat8.get p
  Nat8.setAt p 0 2
  y = Nat8.getAt p 0
  Ptr.free p
  (x == 1, y == 2)

nat16Test = do
  p = Nat16.allocate 1
  Nat16.set p 1
  x = Nat16.get p
  Nat16.setAt p 0 2
  y = Nat16.getAt p 0
  Ptr.free p
  (x == 1, y == 2)

nat32Test = do
  p = Nat32.allocate 1
  Nat32.set p 1
  x = Nat32.get p
  Nat32.setAt p 0 2
  y = Nat32.getAt p 0
  Ptr.free p
  (x == 1, y == 2)

nat64Test = do
  p = Nat.allocate 1
  Nat.set p 1
  x = Nat.get p
  Nat.setAt p 0 2
  y = Nat.getAt p 0
  Ptr.free p
  (x == 1, y == 2)

ptrTest = do
  p = Ptr.allocate 1
  Ptr.set p (Ptr.cast p)
  q = Ptr.cast (Ptr.get p)
  Ptr.setAt p 0 (Ptr.cast p)
  r = Ptr.cast (Ptr.getAt p 0)
  Ptr.free p
  (p == q, p == r)

baTest = do
  pinned = IO.pinnedByteArray 10
  p = PinnedByteArray.contents pinned

  go = cases
    n | n < 10 ->
        Nat8.setAt p n n
        go (n+1)
      | otherwise -> ()

  keepAlive pinned '(go 0)

  read acc = cases
    i | i < 10 -> read (acc :+ Nat8.getAt p i) (i+1)
      | otherwise -> acc

  keepAlive pinned '(read [] 0)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + baTest      : '{IO} [Nat]
  + float32Test : '{IO} (Boolean, Boolean)
  + floatTest   : '{IO} (Boolean, Boolean)
  + int16Test   : '{IO} (Boolean, Boolean)
  + int32Test   : '{IO} (Boolean, Boolean)
  + int64Test   : '{IO} (Boolean, Boolean)
  + int8Test    : '{IO} (Boolean, Boolean)
  + nat16Test   : '{IO} (Boolean, Boolean)
  + nat32Test   : '{IO} (Boolean, Boolean)
  + nat64Test   : '{IO} (Boolean, Boolean)
  + nat8Test    : '{IO} (Boolean, Boolean)
  + ptrTest     : '{IO} (Boolean, Boolean)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/ptr> run floatTest

  (true, true)

scratch/ptr> run float32Test

  (true, true)

scratch/ptr> run int8Test

  (true, true)

scratch/ptr> run int16Test

  (true, true)

scratch/ptr> run int32Test

  (true, true)

scratch/ptr> run int64Test

  (true, true)

scratch/ptr> run nat8Test

  (true, true)

scratch/ptr> run nat16Test

  (true, true)

scratch/ptr> run nat32Test

  (true, true)

scratch/ptr> run nat64Test

  (true, true)

scratch/ptr> run ptrTest

  (true, true)

scratch/ptr> run baTest

  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```
