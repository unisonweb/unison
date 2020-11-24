
Test for code serialization operations.

Define a function, serialize it, then deserialize it back to an actual
function. Also ask for its dependencies for display later.

```unison
type Three a b c = zero a | one b | two c

ability Zap where
  zap : Three Nat Nat Nat

concatMap : (a -> [b]) -> [a] -> [b]
concatMap f = cases
  [] -> []
  x +: xs -> f x ++ concatMap f xs

h : Three Nat Nat Nat -> Nat -> Nat
h y x = match y with
  zero y -> x + y
  one y -> x + y + y
  two y -> x + 3*y

f : Nat ->{Zap} Nat
f x = h zap x

fVal : Value
fVal = Value.value f

fDeps : [Term]
fDeps = Value.dependencies fVal

fSer : Bytes
fSer = Value.serialize fVal

g : '{io2.IO} (Nat ->{Zap} Nat)
g = 'match Value.deserialize fSer with
  Left tx -> bug tx
  Right v -> match Value.load v with
    Left _ -> bug "missing deps"
    Right func -> func

zapper : Request {Zap} r -> r
zapper = cases
  { r } -> r
  { zap -> k } -> handle k (zero 5) with zapper

x : '{IO} Nat
x _ = handle !g 5 with zapper

void : '{IO} a -> '{IO} ()
void x = 'let
  y = !x
  ()

main : '{IO} ()
main = void x
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Three a b c
      ability Zap
      concatMap : (a ->{g} [b]) ->{g} [a] ->{g} [b]
      f         : Nat ->{Zap} Nat
      fDeps     : [Term]
      fSer      : Bytes
      fVal      : Value
      g         : '{IO} (Nat ->{Zap} Nat)
      h         : Three Nat Nat Nat -> Nat -> Nat
      main      : '{IO} ()
      void      : '{IO} a -> '{IO} ()
      x         : '{IO} Nat
      zapper    : Request {Zap} r -> r

```
This simply runs some functions to make sure there isn't a crash. Once
we gain the ability to capture output in a transcript, it can be modified
to actual show that the serialization works.

```ucm
.> add

  ⍟ I've added these definitions:
  
    type Three a b c
    ability Zap
    concatMap : (a ->{g} [b]) ->{g} [a] ->{g} [b]
    f         : Nat ->{Zap} Nat
    fDeps     : [Term]
    fSer      : Bytes
    fVal      : Value
    g         : '{IO} (Nat ->{Zap} Nat)
    h         : Three Nat Nat Nat -> Nat -> Nat
    main      : '{IO} ()
    void      : '{IO} a -> '{IO} ()
    x         : '{IO} Nat
    zapper    : Request {Zap} r -> r

.> display fDeps

  [termLink f]

.> run main

```
