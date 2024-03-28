# Non-empty Vectors

![Build Status](https://github.com/emilypi/nonempty-vector/workflows/CI/badge.svg) [![Hackage](https://img.shields.io/hackage/v/nonempty-vector.svg)](https://hackage.haskell.org/package/nonempty-vector)

This package presents thin wrappers around mutable and immutable [Data.Vector](https://hackage.haskell.org/package/vector) types. The entire Vector API is supported for both sets of boxed vectors, with future plans to support unboxed, primitive, storable, and generic vectors.

There are no external dependencies that are not already in `base`.

### Motivation

Every "container" in the Haskell ecosystem features a [non-empty variant](https://hackage.haskell.org/package/nonempty-containers), including the venerable [list](https://hackage.haskell.org/package/semigroups), aside from `vector`. Many (including myself) use `vector` for its incredible performance characteristics achieved over many years by the CLC and authors of the library. But many of us also want to adhere to the principle of least power, and not have to worry about whether `head` or `tail` (for example) are safe. This package addresses both of the previous points. No new pointer indirection is exposed by this library except at construction (and even then - `unsafe` constructors are supplied), with as much reuse of `vector`'s library as possible to make sure asymptotics stay the same.
