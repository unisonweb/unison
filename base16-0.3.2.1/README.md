# Base16

![Build Status](https://github.com/emilypi/base16/workflows/Haskell-CI/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/base16.svg)](https://hackage.haskell.org/package/base16)

RFC 4648-compliant Base16 encoding and decoding.

For the companion optics and pattern synonyms, see [base16-lens](https://hackage.haskell.org/package/base16-lens).

### Summary

The following types have supported codecs:

- `Data.ByteString`
- `Data.ByteString.Lazy`
- `Data.ByteString.Short`
- `Data.Text`
- `Data.Text.Lazy`
- `Data.Text.Short`

Additionally this library has

- Much better performance than `base16-bytestring` for encode and decode, with a more conventional api.
- Support for mixed-case hex decoding (defaults to lower-case encoding by convention)
- Optics for handling more complex structures with Base16 representations via the `base16-lens` package
- Checks for both validity and correctness of Base16 encodings.

There are no dependencies aside from those bundled with GHC.
