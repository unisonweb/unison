# Changelog for [`old-time` package](http://hackage.haskell.org/package/old-time)

## 1.1.1.0  *Mar 2026*

  * Workaround DST issues in `toClockTime . toUTCTime`, at least in some cases.
  * Cabalize test suite.
  * Document issues with `TimeDiff`.

## 1.1.0.5  *Jan 2026*

  * Remove unused `includes` field in `.cabal` file
    (Simon Hengel, [PR #17](https://github.com/haskell/old-time/pull/17)).
  * Bump `cabal-version` to 1.18.
  * Build tested with GHC 8.0 - 9.14.1.

## 1.1.0.4  *Dec 2023*

  * Add `build-tools: hsc2hs` to `old-time.cabal` for compilation with GHC 8.
    Fixes [issue #12](https://github.com/haskell/old-time/issues/12)

## 1.1.0.3  *Nov 2014*

  * Decoupled from GHC distribution

## 1.1.0.2  *Mar 2014*

  * Bundled with GHC 7.8.1

  * Supports `base-4.7.0.0`

  * Remove NHC98-specific code

  * Update to Cabal 1.10 format

## 1.1.0.1  *Sep 2012*

  * Bundled with GHC 7.6.1

  * Don't include deprecated `<sys/timeb.h>` on FreeBSD

  * Fix `gettimeofday(2)` call on Win64
