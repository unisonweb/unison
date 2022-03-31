
Some random ability stuff to ensure things work.

```unison
unique ability A where
  one : Nat ->{A} Nat
  two : Nat -> Nat ->{A} Nat
  three : Nat -> Nat -> Nat ->{A} Nat
  four : Nat ->{A} (Nat -> Nat -> Nat -> Nat)

ha : Request {A} r -> r
ha = cases
  { x } -> x
  { one i -> c } -> handle c (i+1) with ha
  { two i j -> c } -> handle c (i+j) with ha
  { three i j k -> c } -> handle c (i+j+k) with ha
  { four i -> c } -> handle c (j k l -> i+j+k+l) with ha
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique ability A
      ha : Request {A} r -> r

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique ability A
    ha : Request {A} r -> r

```
