See also http://pvp.haskell.org/faq

## 0.94.0.3

_2025-03-02 Andreas Abel_

- Drop support for GHC 7
- Make `Prelude` imports explicit, add `LANGUAGE NoImplicitPrelude`
- Make upper bounds of dependencies major-major (all are shipped with GHC)
- Tested with GHC 8.0 - 9.12.1

## 0.94.0.2 Revision 1

_2022-05-25 Andreas Abel_

- Allow `base-4.17` and higher, for GHC 9.4

## 0.94.0.2

_2021-11-16 Andreas Abel_

- Allow `text-2.0`
- Remove unused dependency `mtl`
- Warning free up to GHC 9.2
- Tested with GHC 7.0 - 9.2

## 0.94.0.1 Revision 1

_2021-08-12 Andreas Abel_

- Allow `base-4.16`, for GHC 9.2

## 0.94.0.1

_2021-02-20 Andreas Abel_

- Workaround for `{-# LANGUAGE Haskell2010 #-}` parser regression introduced in GHC 9.0
- Optimization flag `-O2` has been removed

## 0.94.0.0

_2019-09-25 Herbert Valerio Riedel_

- **Breaking change**: Switch RegExp API from the previously used `Monad(fail)` to `MonadFail(fail)` to denote matching failures
- Define `Extract Text` instances for strict and lazy `Text` types
- Compatibility with `base-4.13.0`
- Explicitly declare all modules `Safe` under SafeHaskell for GHC 7.4 and higher

----
