### `debug.file`
I can use `debug.file` to see the hashes of the last typechecked file.

Given this .u file:
```unison
structural type outside.A = A Nat outside.B
structural type outside.B = B Int
outside.c = 3
outside.d = c < (p + 1)

structural type inside.M = M outside.A
inside.p = c
inside.q x = x + p * p
inside.r = d
```

```ucm
.> debug.file

  type inside.M#goba2va40r
  type outside.A#ihqhr4prbp
  type outside.B#mm8h095nrg
  inside.p#h63obi5rb4
  inside.q#1qtbral9uo
  inside.r#9guss29ljv
  outside.c#fs7la111vn
  outside.d#p7dvt0ka99

```
This will help me make progress in some situations when UCM is being deficient or broken.

### `dependents` / `dependencies`
But wait, there's more.  I can check the dependencies and dependents of a definition:
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type inside.M
    structural type outside.A
    structural type outside.B
    inside.p  : Nat
    inside.q  : Nat -> Nat
    inside.r  : Boolean
    outside.c : Nat
    outside.d : Boolean

.> dependents q

  #1qtbral9uo doesn't have any named dependents.

.> dependencies q

  Dependencies of #1qtbral9uo:
  
       Reference   Name
    1. ##Nat       builtin.Nat
    2. ##Nat.*     builtin.Nat.*
    3. ##Nat.+     builtin.Nat.+
    4. #h63obi5rb4 inside.p

.> dependencies B

  Dependencies of #mm8h095nrg:
  
       Reference Name
    1. ##Int     builtin.Int

  Dependencies of #mm8h095nrg#0:
  
       Reference   Name
    1. #mm8h095nrg outside.B
    2. ##Int       builtin.Int

.> dependencies d

  Dependencies of #p7dvt0ka99:
  
       Reference     Name
    1. ##Boolean     builtin.Boolean
    2. ##Nat         builtin.Nat
    3. ##Nat.+       builtin.Nat.+
    4. ##Universal.< builtin.Universal.<
    5. #fs7la111vn   outside.c
    6. #h63obi5rb4   inside.p

.> dependents d

  Dependents of #p7dvt0ka99:
  
       Reference   Name
    1. #9guss29ljv inside.r

```
We don't have an index for dependents of constructors, but iirc if you ask for that, it will show you dependents of the structural type that provided the constructor.
