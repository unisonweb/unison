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

  type inside.M#h37a56c5ep
  type outside.A#6l6krl7n4l
  type outside.B#eo6rj0lj1b
  inside.p#htoo5rnb54
  inside.q#vtdbqaojv6
  inside.r#nkgohbke6n
  outside.c#f3lgjvjqoo
  outside.d#ukd7tu6kds

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

  #vtdbqaojv6 doesn't have any named dependents.

.> dependencies q

  Dependencies of #vtdbqaojv6:
  
       Reference   Name
    1. ##Nat       builtin.Nat
    2. ##Nat.*     builtin.Nat.*
    3. ##Nat.+     builtin.Nat.+
    4. #htoo5rnb54 inside.p

.> dependencies B

  Dependencies of #eo6rj0lj1b:
  
       Reference Name
    1. ##Int     builtin.Int

  Dependencies of #eo6rj0lj1b#0:
  
       Reference   Name
    1. #eo6rj0lj1b outside.B
    2. ##Int       builtin.Int

.> dependencies d

  Dependencies of #ukd7tu6kds:
  
       Reference     Name
    1. ##Boolean     builtin.Boolean
    2. ##Nat         builtin.Nat
    3. ##Nat.+       builtin.Nat.+
    4. ##Universal.< builtin.Universal.<
    5. #f3lgjvjqoo   outside.c
    6. #htoo5rnb54   inside.p

.> dependents d

  Dependents of #ukd7tu6kds:
  
       Name     Reference
    1. inside.r #nkgohbke6n
  
  Tip: Try `view 1` to see the source of any numbered item in
       the above list.

```
We don't have an index for dependents of constructors, but iirc if you ask for that, it will show you dependents of the structural type that provided the constructor.
