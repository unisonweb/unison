Version history for `data-default`
==================================

## 0.8.0.2 -- 2026-01-06

- Make the code compatible with MicroHs (which basically means removing the
  tuple instances of size 16+ as those don't exist in MicroHs). This does not
  affect the GHC version of this module, which is unchanged.

- Relax upper version bound on containers from `<0.8` to `<0.9` (technically
  this change was already applied to version 0.8.0.1 on hackage).

## 0.8.0.1 -- 2025-03-16

- Add a changelog file.

- Add an official bug tracker.

- Switch upstream to codeberg.org.

## 0.8.0.0 -- 2024-10-23

- The split into multiple packages (`data-default-class`,
  `data-default-instances-*`) is gone. There is only
  `data-default` now (with no dependencies on
  `data-default-class` or anything).

- `data-default` only depends on `containers` and `base` now (but
  requires `base >= 4.8`).

- The `Default` instances for `Data.DList` and `System.Locale`
  are gone (but could be revived with updated versions of
  `data-default-instances-dlist` and
  `data-default-instances-old-locale` in the future?).

- The `Default` instances for `e -> a` and `IO a` have been
  removed.

- The "generic" default `Default` instance now supports sum types
  as well (by always choosing the left constructor).

- The internal `GDefault` type is now semi-public (in the
  `Data.Default.Internal` module) and has stub documentation.

- New instances:

  - Tuples of size up to 31 (previously 7)

  - `Identity`

  - `Const`

  - `Proxy`

  - `Solo`

  - `Bool`

  - `CBool`

  - `Ptr`

  - `ConstPtr`

  - `FunPtr`

  - `IntPtr`

  - `WordPtr`

## 0.7.1.3 -- 2024-10-17

- Add upper version bounds on all dependencies.

- Add cabal description.

## 0.7.1.2 -- 2024-10-13

- Add proper haddock module headers in each module.

- Update `.cabal` file to `cabal-version: 3.0` format.

- No changes to the license itself, but keep the
  wording/formatting consistent in all license
  files/headers/tags.

- Make the (rudimentary) test runnable by cabal and add it to the
  `.cabal` file.

## 0.7.1.1 -- 2016-06-25

- Remove `data-default-instances-base` dependency.

## 0.7.1 -- 2016-06-12

- Support `GHC.Generics` in ghc 7.4 (previously: 7.6+).

## 0.7.0 -- 2016-05-21

- Add instances for the numeric types from `Foreign.C.Types`.

## 0.6.0 -- 2016-04-17

- Add support for `GHC.Generics` in ghc 7.6+.

## 0.5.2 -- 2016-04-02

- Split code into several packages (`data-default-class`,
  `data-default-instances-*`), which `data-default` simply
  re-exports.

## 0.5.1 -- 2013-03-07

- Add instances for tuples of length 6 and 7.

## 0.5.0 -- 2012-06-18

- Add instances for types in `Data.Int` and `Data.Word`.

## 0.4.0 -- 2012-03-30

- Add `Complex` instance.

- Add `TimeLocale` instance (from `old-locale`).

## 0.3.0 -- 2011-08-08

- Add instances for `IntMap`, `IntSet`, `Seq`, and `Tree` (from
  `containers`).

- Add internal basic tests.

- Track changes in git.

- Add instances for `Ordering`, `All`, `Any`, `Dual`, `Endo`,
  `Sum`, `Product`. `First`, `Last`, `DList`.

- Add instances for tuples of length 2 to 5.

## 0.2.0.1 -- 2011-01-02

- Add `LICENSE` file.

- Add version bounds on `base` dependency.

## 0.2 -- 2008-05-21

- Initial upload.

- Instances provided: `Double`, `Float`, `Int`, `Integer`, `()`,
  `[a]`, `Ratio`, `IO`, `Maybe`, `Set`, `e -> a`, `Map`.
