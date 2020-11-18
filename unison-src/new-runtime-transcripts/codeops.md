
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

x : '{IO} Nat
x _ = !g 5

main : '{IO} ()
main = 'let
  y = !x
  ()
```

This simply runs some functions to make sure there isn't a crash. Once
we gain the ability to capture output in a transcript, it can be modified
to actual show that the serialization works.

```ucm
.> add
.> display fDeps
.> run main
```
