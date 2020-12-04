Test for new Text -> Bytes conversions explicitly using UTF-8 as the encoding

Unison has function for converting between `Text` and a UTF-8 `Bytes` encoding of the Text.

```ucm
.> find Utf8

  1. builtin.Text.fromUtf8 : Bytes -> Either Failure Text
  2. builtin.Text.toUtf8 : Text -> Bytes
  

```
ascii characters are encoded as single bytes (in the range 0-127).

```unison
ascii: Text
ascii = "ABCDE"

> toUtf8 ascii

```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ascii : Text
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    4 | > toUtf8 ascii
          ⧩
          0xs4142434445

```
non-ascii characters are encoded as multiple bytes.

```unison
greek: Text
greek = "ΑΒΓΔΕ"

> toUtf8 greek
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      greek : Text
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    4 | > toUtf8 greek
          ⧩
          0xsce91ce92ce93ce94ce95

```
We can check that encoding and then decoding should give us back the same `Text` we started with 

```unison
checkRoundTrip: Text -> [Result]
checkRoundTrip t = 
  bytes = toUtf8 t
  match fromUtf8 bytes with 
    Left e -> [Result.Fail "could not decode"]
    Right t' -> if t == t' then [Result.Ok "Passed"] else [Result.Fail ("Got: " ++ t' ++ " Expected: " ++ t)]

greek = "ΑΒΓΔΕ"

test> checkRoundTrip greek
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      checkRoundTrip : Text -> [Result]
      greek          : Text
    test.kqfpde2g5a  (Unison bug, unknown term)
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    10 | test> checkRoundTrip greek
    
    ✅ Passed Passed

```
If we try to decode an invalid set of bytes, we get back `Text` explaining the decoding error:

```unison
greek_bytes = Bytes.fromList [206, 145, 206, 146, 206, 147, 206, 148, 206]


-- Its an error if we drop the first byte
> match fromUtf8 (drop 1 greek_bytes) with
  Left (Failure _ t) -> t

```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      greek_bytes : Bytes
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    5 | > match fromUtf8 (drop 1 greek_bytes) with
          ⧩
          "Cannot decode byte '\\x91': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"

```
