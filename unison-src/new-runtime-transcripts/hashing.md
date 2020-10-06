# Hashing and HMAC builtins

```ucm:hide
.> builtins.merge
.> cd builtin
```

Unison has some cryptographic builtins for hashing and computing [HMACs](https://en.wikipedia.org/wiki/HMAC) (hash-based message authentication codes). This transcript shows their usage and has some test cases.

## Setup

You can skip this section, it's just needed to make the transcript self-contained. In order to print out and test these hashes we will be using some builtins for base16 (aka hexidecimal) encoding and decoding.

```ucm
.builtin> ls Bytes
```

Notice the `fromBase16` and `toBase16` functions. Here's some (somewhat inefficient) convenience functions for converting `Bytes` to and from base 16 `Text`. These could be replaced by use of `Text.toUtf8` and `Text.tryFromUtf8` once those builtins exist:

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

check : Boolean -> [Result]
check = cases
  true  -> [Ok "Passed."]
  false -> [Fail "Failed."]

```

The test shows that `hex (fromHex str) == str` as expected.

```ucm
.scratch> add
```

## API overview

Here's a few examples.

```unison
ex1 = fromHex "2947db"
        |> crypto.hashBytes Sha3_512
        |> hex

ex2 = fromHex "02f3ab"
        |> crypto.hashBytes Blake2b_256
        |> hex

mysecret : Bytes
mysecret = fromHex "237be2"

ex3 = fromHex "50d3ab"
        |> crypto.hmacBytes Sha2_256 mysecret
        |> hex

> ex1
> ex2
> ex3
```

And here's the full API:

```ucm
.builtin.crypto> find
```

Note that the universal versions of `hash` and `hmac` are currently unimplemented and will bomb at runtime:

```unison:error
> crypto.hash Sha3_256 (fromHex "3849238492")
```

## Tests


