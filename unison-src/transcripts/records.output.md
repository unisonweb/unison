Ensure that Records keep their syntax after being added to the codebase

## Record with 1 field

``` unison
unique type Record1 = { a : Text }
```

``` ucm
scratch/main> view Record1

  type Record1 = { a : Text }

```
## Record with 2 fields

``` unison
unique type Record2 = { a : Text, b : Int }
```

``` ucm
scratch/main> view Record2

  type Record2 = { a : Text, b : Int }

```
## Record with 3 fields

``` unison
unique type Record3 = { a : Text, b : Int, c : Nat }
```

``` ucm
scratch/main> view Record3

  type Record3 = { a : Text, b : Int, c : Nat }

```
## Record with many fields

``` unison
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

``` ucm
scratch/main> view Record4

  type Record4
    = { a : Text,
        b : Int,
        c : Nat,
        d : Bytes,
        e : Text,
        f : Nat,
        g : [Nat] }

```
## Record with many many fields

``` unison
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

``` ucm
scratch/main> view Record5

  type Record5
    = { zero : Nat,
        one : [Nat],
        two : [[Nat]],
        three : [[[Nat]]],
        four : [[[[Nat]]]],
        five : [[[[[Nat]]]]],
        six : [[[[[[Nat]]]]]],
        seven : [[[[[[[Nat]]]]]]],
        eight : [[[[[[[[Nat]]]]]]]],
        nine : [[[[[[[[[Nat]]]]]]]]],
        ten : [[[[[[[[[[Nat]]]]]]]]]],
        eleven : [[[[[[[[[[[Nat]]]]]]]]]]],
        twelve : [[[[[[[[[[[[Nat]]]]]]]]]]]],
        thirteen : [[[[[[[[[[[[[Nat]]]]]]]]]]]]],
        fourteen : [[[[[[[[[[[[[[Nat]]]]]]]]]]]]]],
        fifteen : [[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]],
        sixteen : [[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]],
        seventeen : [[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]],
        eighteen : [[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]],
        nineteen : [[[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]]],
        twenty : [[[[[[[[[[[[[[[[[[[[Nat]]]]]]]]]]]]]]]]]]]] }

```
## Record with user-defined type fields

This record type has two fields whose types are user-defined (`Record4` and `UserType`).

``` unison
unique type UserType = UserType Nat

unique type RecordWithUserType = { a : Text, b : Record4, c : UserType }
```

If you `view` or `edit` it, it *should* be treated as a record type, but it does not (which is a bug)

``` ucm
scratch/main> view RecordWithUserType

  type RecordWithUserType
    = { a : Text, b : Record4, c : UserType }

```
## Syntax

Trailing commas are allowed.

``` unison
unique type Record5 =
  { a : Text,
    b : Int,
  }
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
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
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Record5

```
