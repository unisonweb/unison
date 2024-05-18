Ensure that Records keep their syntax after being added to the codebase

```ucm:hide
.> builtins.merge
.> load unison-src/transcripts-using-base/base.u
```

## Record with 1 field

```unison:hide
unique type Record1 = { a : Text }
```

```ucm:hide
.> add
```

```ucm
.> view Record1
```

## Record with 2 fields

```unison:hide
unique type Record2 = { a : Text, b : Int }
```

```ucm:hide
.> add
```

```ucm
.> view Record2
```

## Record with 3 fields

```unison:hide
unique type Record3 = { a : Text, b : Int, c : Nat }
```

```ucm:hide
.> add
```

```ucm
.> view Record3
```

## Record with many fields

```unison:hide
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

```ucm:hide
.> add
```

```ucm
.> view Record4
```

## Record with many many fields

```unison:hide
unique type Record5 = {
  zero : Nat,
  one : [Nat],
  two : [[Nat]],
  three: [[[Nat]]],
  four: [[[[Nat]]]],
  five: [[[[[Nat]]]]],
  six: [[[[[[Nat]]]]]],
  seven: [[[[[[[Nat]]]]]]],
  eight: [[[[[[[[Nat]]]]]]]],
  nine: [[[[[[[[[Nat]]]]]]]]],
  ten: [[[[[[[[[[Nat]]]]]]]]]],
  eleven: [[[[[[[[[[[Nat]]]]]]]]]]],
  twelve: [[[[[[[[[[[[Nat]]]]]]]]]]]],
  thirteen: [[[[[[[[[[[[[Nat]]]]]]]]]]]]],
  fourteen: [[[[[[[[[[[[[[Nat]]]]]]]]]]]]]],
  fifteen: [[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]],
  sixteen: [[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]],
  seventeen: [[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]],
  eighteen: [[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]],
  nineteen: [[[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]]],
  twenty: [[[[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]]]]
}
```

```ucm:hide
.> add
```

```ucm
.> view Record5
```

## Record with user-defined type fields

This record type has two fields whose types are user-defined (`Record4` and `UserType`).

```unison:hide
unique type UserType = UserType Nat

unique type RecordWithUserType = { a : Text, b : Record4, c : UserType }
```

```ucm:hide
.> add
```

If you `view` or `edit` it, it _should_ be treated as a record type, but it does not (which is a bug)

```ucm
.> view RecordWithUserType
```


## Syntax

Trailing commas are allowed.

```unison
unique type Record5 =
  { a : Text,
    b : Int,
  }
```
