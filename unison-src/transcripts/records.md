Ensure that Records keep their syntax after being added to the codebase 

```ucm:hide
.> builtins.mergeio
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

## Syntax

Trailing commas are allowed.

```unison
unique type Record5 = 
  { a : Text, 
    b : Int,
  }
```
