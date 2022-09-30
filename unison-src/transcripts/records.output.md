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
## Syntax

Trailing commas are allowed.

```unison
unique type Record5 = 
  { a : Text, 
    b : Int,
  }
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Record5
      Record5.a        : Record5 -> Text
      Record5.a.modify : (Text ->{g} Text)
                         -> Record5
                         ->{g} Record5
      Record5.a.set    : Text -> Record5 -> Record5

```
