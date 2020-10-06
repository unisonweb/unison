# Hashing and HMAC builtins

```ucm:hide
.> builtins.merge
.> cd builtin
```

Unison has some cryptographic builtins for hashing and computing [HMACs](https://en.wikipedia.org/wiki/HMAC) (hash-based message authentication codes). In order to print out and test these hashes we will be using some new primitives for base16 encoding and decoding:

```ucm
.builtin> ls Bytes
```

```unison
a |> f = f a

List.map f as =
  go acc = cases
    [] -> acc
    (h +: t) -> go (acc :+ f h) t
  go [] as

-- not very efficient, but okay for testing
Bytes.hex : Bytes -> Text
Bytes.hex b =
  Bytes.toBase16 b
    |> Bytes.toList
    |> List.map Char.fromNat
    |> Text.fromCharList

Bytes.fromHex : Text -> Bytes
Bytes.fromHex txt =
  match Text.toCharList txt
          |> List.map Char.toNat
          |> Bytes.fromList
          |> Bytes.fromBase16
  with
    Left e -> bug e
    Right bs -> bs

> hex (fromHex "18293857103947109284ff")
```

```ucm
.builtin.crypto> find
```



