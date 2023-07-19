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
## Record with user-defined type fields

This record type has two fields whose types are user-defined (`Record4` and `UserType`).

```unison
unique type UserType = UserType Nat

unique type RecordWithUserType = { a : Text, b : Record4, c : UserType }
```

If you `view` or `edit` it, it _should_ be treated as a record type, but it does not (which is a bug)

```ucm
.> view RecordWithUserType

  unique type RecordWithUserType
    = { a : Text, b : Record4, c : UserType }

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
      Record5.b        : Record5 -> Int
      Record5.b.modify : (Int ->{g} Int)
                         -> Record5
                         ->{g} Record5
      Record5.b.set    : Int -> Record5 -> Record5

```
