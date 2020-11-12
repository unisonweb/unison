
Test for code serialization operations.

Define a function, serialize it, then deserialize it back to an actual
function. Also ask for its dependencies for display later.

```unison
f : Nat -> Nat
f x = x + 5

fVal : Value
fVal = Value.value f

fDeps : [Term]
fDeps = Value.dependencies fVal

fSer : Bytes
fSer = Value.serialize fVal

g : '{io2.IO} (Nat -> Nat)
g = 'match Value.deserialize fSer with
  Left tx -> bug tx
  Right v -> match Value.load v with
    Left _ -> bug "missing deps"
    Right func -> func
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      f     : Nat -> Nat
      fDeps : [Term]
      fSer  : Bytes
      fVal  : Value
      g     : '{IO} (Nat -> Nat)

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    f     : Nat -> Nat
    fDeps : [Term]
    fSer  : Bytes
    fVal  : Value
    g     : '{IO} (Nat -> Nat)

```
This takes advantage of an exhaustiveness loophole to run the IO code.
Ideally it'd be better to be able to view some IO stuff in a transcript.

```unison
loophole : Request {io2.IO} a -> a
loophole = cases
  { a } -> a

x : Nat
x = handle !g 5 with loophole
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      loophole : Request {IO} a -> a
      x        : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    loophole : Request {IO} a -> a
    x        : Nat

.> display fDeps

  [termLink f]

.> display x

  10

```
