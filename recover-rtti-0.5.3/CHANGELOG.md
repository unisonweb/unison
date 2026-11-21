# Revision history for recover-rtti

## 0.5.3 -- 2026-01-07

* Support `ghc-9.14` (Brandon Chinn, #48)
* Support `containers-0.8` (Brandon Chinn, #49)

## 0.5.2 -- 2025-12-16

* Support `unordered-containers-0.2.21` (Brandon Chinn, #45)

## 0.5.1 -- 2025-07-19

* Relax bounds, including support for `QuickCheck-2.16`
* Drop support for `ghc < 9.2`

## 0.5.0 -- 2024-06-12

* Support ghc 9.8 and 9.10, including bytestring 0.12
* Support `ByteArray` and `MutableByteArray`
* Relax bounds (Marcin Szamotulski, #38)
* Drop support for ghc 8.8

## 0.4.3 -- 2023-06-05

* Support aeson 2.1, vector 0.13, and primitive 0.8
  (Mitchell Rosen, #29, #35, #36)
* Support bytestring 0.11.4
* Support ghc 9.4 and 9.6

## 0.4.2 -- 2023-03-23

* Support mtl 2.3 (requiring at least 2.3.1) (#26).
  Thanks to Rebecca Turner.

## 0.4.1 -- 2022-03-17

* Support for ghc 9.2, bytestring 0.11, aeson 0.2
* Added `traceAnything` and `traceAnythingId` (#13)
* Added deriving-via support (`AnythingToString`, `BoxAnything`) (#3)

## 0.4 -- 2021-06-30

* Correctly set some required lower bounds.
* Add support for reclassification
* Add classification equality check
* Add support for primitive arrays and vectors
* Fix classification on OSX
* General internal cleanup of the library

This release is backwards incompatible with 0.3, but users that simply use
`anythingToString` should be unaffected.

## 0.3.0.0 -- 2021-03-17

* Fix bug that could cause `anythingToString` to fail on lists with an
  `elemUserDefined` exception (issue #6). Thanks to Bit Connor for the report!

## 0.2.1.0 -- 2021-03-17

* Add support for unordered-containers and boxed vectors.

## 0.2.0.0 -- 2021-03-15

* Reorganize module structure (primarily to improve Haddocks)

## 0.1.0.0 -- 2021-03-11

* Alpha release
