# Duplicate Term Detection

```ucm:hide
.> builtins.merge
```


Trivial duplicate terms should be detected:

```unison:error
x = 1
x = 2
```

Equivalent duplicate terms should be detected:

```unison:error
x = 1
x = 1
```

Duplicates from record accessors/setters should be detected

```unison:error
structural type Record = {x: Nat, y: Nat}
Record.x = 1
Record.x.set = 2
Record.x.modify = 2
```

Duplicate terms and constructors should be detected:

```unison:error
structural type SumType = X

SumType.X = 1

structural ability AnAbility where
  thing : Nat -> ()

AnAbility.thing = 2
```
