[![Hackage version](https://img.shields.io/hackage/v/blaze-builder.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/blaze-builder)
[![blaze-builder on Stackage Nightly](https://stackage.org/package/blaze-builder/badge/nightly)](https://stackage.org/nightly/package/blaze-builder)
[![Stackage LTS version](https://www.stackage.org/package/blaze-builder/badge/lts?label=Stackage)](https://www.stackage.org/package/blaze-builder)
[![Cabal build](https://github.com/blaze-builder/blaze-builder/workflows/Haskell-CI/badge.svg)](https://github.com/blaze-builder/blaze-builder/actions)

blaze-builder
=============

This library allows to efficiently serialize Haskell values to lazy bytestrings
with a large average chunk size. The large average chunk size allows to make
good use of cache prefetching in later processing steps (e.g. compression) and
reduces the system call overhead when writing the resulting lazy bytestring to a
file or sending it over the network.

This library was inspired by the module `Data.Binary.Builder` provided by the
`binary` package. It was originally developed with the specific needs of the
`blaze-html` package in mind. Since then it has been restructured to serve as a
drop-in replacement for `Data.Binary.Builder`, which it improves upon both in
speed as well as expressivity.

To see the improvements in speed, run the throughput benchmark, which measures
serialization speeds for writing `Word8`, `Word16`, `Word32` and `Word64` in different
endian formats and different chunk sizes, using the command
```
  make bench-throughput
```
or run the list serialization comparison benchmark
```
  make bench-blaze-vs-binary
```
Checkout the combinators in the module `Blaze.ByteString.Builder.Write` to see
the improvements in expressivity. This module allows to incorporate efficient
primitive buffer manipulations as parts of a builder. We use this facility
in the `blaze-html` HTML templating library to allow for the efficient
serialization of HTML escaped and UTF-8 encoded characters.
