The `vector` package [![Build Status](https://github.com/haskell/vector/workflows/CI/badge.svg)](https://github.com/haskell/vector/actions?query=branch%3Amaster)
====================

Vector is a collection of efficient `Int`-indexed array implementations: 
[boxed, unboxed, storable, and primitive vectors](#vectors-available-in-the-package)
(all can be mutable or immutable). The package features a generic API,
polymorphic in vector type, and implements [*stream fusion*](#stream-fusion), 
a powerful optimisation framework that can help eliminate intermediate data structures.

## Table of Contents

<!-- no toc -->
- [Tutorial](#tutorial)
- [Vector vs Array](#vector-vs-array)
- [Vectors Available in the Package](#vectors-available-in-the-package)
- [Stream Fusion](#stream-fusion)

## Tutorial

A beginner-friendly tutorial for vectors can be found on 
[MMHaskell](https://mmhaskell.com/data-structures/vector).


If you have already started your adventure with vectors, 
the tutorial on [Haskell Wiki](https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial) 
covers more ground.

## Vector vs Array

Arrays are data structures that can store a multitude of elements 
and allow immediate access to every one of them. However, they are 
often seen as legacy constructs that are rarely used in modern Haskell.
Even though Haskell has a built-in [Data.Array module](https://hackage.haskell.org/package/array-0.5.7.0), 
arrays might be a bit overwhelming to use due to their complex API. 
Conversely, vectors incorporate the array’s *O(1)* access to elements 
with a much friendlier API of lists. Since they allow for framework 
optimisation via loop fusion, vectors emphasise efficiency and keep 
a rich interface. Unless you’re confident with arrays, it’s 
well-advised to use vectors when looking for a similar functionality.

## Vectors Available in the Package

**Lazy boxed vectors** (`Data.Vector`) store each of their elements as a 
pointer to a heap-allocated value. Because of indirection, lazy boxed vectors
are slower in comparison to unboxed vectors.

**Strict boxed vectors** (`Data.Vector.Strict`) contain elements that are 
[strictly evaluated](https://tech.fpcomplete.com/haskell/tutorial/all-about-strictness/).

**Unboxed vectors** (`Data.Vector.Unboxed`) determine an array's representation
from its elements' type. For example, vector of primitive types (e.g. `Int`) will be 
backed by primitive array while vector of product types by structure of arrays.
They are quite efficient due to the unboxed representation they use.

**Storable vectors** (`Data.Vector.Storable`) are backed by pinned memory, i.e., 
they cannot be moved by the garbage collector. Their primary use case is C FFI.  

**Primitive vectors** (`Data.Vector.Primitive`) are backed by simple byte array and 
can store only data types that are represented in memory as a sequence of bytes without
a pointer, i.e., they belong to the `Prim` type class, e.g., `Int`, `Double`, etc.
It's advised to use unboxed vectors if you're looking for the performance of primitive vectors,
but more versality. 
 
## Stream Fusion

An optimisation framework used by vectors, stream fusion is a technique that merges 
several functions into one and prevents creation of intermediate data structures. For example, 
the expression `sum . filter g . map f` won't allocate temporary vectors if 
compiled with optimisations.