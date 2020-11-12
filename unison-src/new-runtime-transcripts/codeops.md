
Test for code serialization operations.

```ucm:hide
.> builtins.merge
.> cd builtin
```

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
.> add
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
.> add
.> display fDeps
.> display x
```
