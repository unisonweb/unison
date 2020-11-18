
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

x : '{IO} Nat
x _ = !g 5

main : '{IO} ()
main = 'let
  y = !x
  ()
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
      main  : '{IO} ()
      x     : '{IO} Nat

```
This simply runs some functions to make sure there isn't a crash. Once
we gain the ability to capture output in a transcript, it can be modified
to actual show that the serialization works.

```ucm
.> add

  ⍟ I've added these definitions:
  
    f     : Nat -> Nat
    fDeps : [Term]
    fSer  : Bytes
    fVal  : Value
    g     : '{IO} (Nat -> Nat)
    main  : '{IO} ()
    x     : '{IO} Nat

.> display fDeps

  [termLink f]

.> run main

```
