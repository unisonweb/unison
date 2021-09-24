Ensure that Records keep their syntax after being added to the codebase 

## Record with 1 field

```unison
unique type Record1 = { a : Text }
```

```ucm
.> view Record1

  unique type Record1 = { a : Text }

```
## Record with 2 fields

```unison
unique type Record2 = { a : Text, b : Int }
```

```ucm
.> view Record2

  unique type Record2 = { a : Text, b : Int }

```
## Record with 3 fields

```unison
unique type Record3 = { a : Text, b : Int, c : Nat }
```

```ucm
.> view Record3

  unique type Record3 = { a : Text, b : Int, c : Nat }

```
## Record with many fields

```unison
unique type Record4 = 
  { a : Text
  , b : Int
  , c : Nat
  , d : Bytes
  , e : Text
  , f : Nat
  , g : [Nat]
  }
```

```ucm
.> view Record4

  unique type Record4
    = { a : Text,
        b : Int,
        c : Nat,
        d : Bytes,
        e : Text,
        f : Nat,
        g : [Nat] }

```
