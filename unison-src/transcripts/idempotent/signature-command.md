# `signature` command

`signature` (alias `sig`) displays the type signature of a definition without showing its full body.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

## Basic usage on builtins

``` ucm
scratch/main> signature lib.builtins.List.map

  lib.builtins.List.map
    : (a ->{e} b) -> [a] ->{e} [b]
```

## User-defined terms

``` unison :hide
myConst : Text -> Nat -> Text
myConst s _ = s

myId : a -> a
myId x = x
```

``` ucm :hide
scratch/main> add
```

`signature` shows the type without the body:

``` ucm
scratch/main> signature myConst

  myConst
    : Text -> Nat -> Text

scratch/main> signature myId

  myId
    : a -> a
```

The `sig` alias works identically:

``` ucm
scratch/main> sig myConst

  myConst
    : Text -> Nat -> Text
```

## Multiple definitions at once

``` ucm
scratch/main> signature myConst myId

  myConst
    : Text -> Nat -> Text
  myId
    : a -> a
```

## User-defined types

``` unison :hide
unique type Color = Red | Green | Blue

unique type Pair a b = Pair a b
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> signature Color

  Color
    (type)

scratch/main> signature Pair

  Pair
    (type)
  Pair.Pair
    : a -> b -> Pair a b
```

## Ability methods

``` unison :hide
ability Store s where
  get : s
  put : s -> ()
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> signature Store.get

  Store.get
    : {Store s} s

scratch/main> signature Store.put

  Store.put
    : s ->{Store s} ()
```

## Error: name not found

``` ucm :error
scratch/main> signature doesNotExist

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    doesNotExist

```

``` ucm :hide
scratch/main> project.delete scratch
```
