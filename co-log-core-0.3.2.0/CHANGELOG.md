# Change log

`co-log-core` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

## 0.3.2.0 — Nov 2, 2022

* [#25](https://github.com/co-log/co-log-core/issues/25):
  Support GHC-9.4.

## 0.3.1.0 — Feb 15, 2022

* [#7](https://github.com/co-log/co-log-core/issues/7):
  Support GHC-9.2.
* [#13](https://github.com/co-log/co-log-core/issues/13):
  Add `WithSeverity` and `mapSeverity` to `Colog.Severity`.

## 🎃 0.3.0.0 — Oct 29, 2021

* [#223](https://github.com/co-log/co-log/pull/223):
  Support GHC-9.0.1.
* [#176](https://github.com/co-log/co-log/issues/176):
  Add `logFlush` action to flush the given `Handle`.

  __Breaking change:__ All `withLog*File` functions how flush handle
  after logging each message. Now you'll see logs in the file
  immediately.

  __Migration guide:__ If you rely on the previous behaviour, then
  copy-paste corresponding functions and remove flushing.

* Update maintainers information to the new
  [Co-Log](https://github.com/co-log) organization.

## 0.2.1.1 — Apr 18, 2020

* [#186](https://github.com/kowainik/co-log/issues/186):
  Support GHC-8.10.1.

## 0.2.1.0 — Jan 19, 2020

* [#139](https://github.com/kowainik/co-log/issues/139):
  Add (unrepresentable) `Functor` instance for `LogAction` with the
  custom type-error.
  (by [@vrom911](https://github.com/vrom911))
* [#148](https://github.com/kowainik/co-log/issues/148):
  Support GHC-8.8.2.
  (by [@chshersh](https://github.com/chshersh))
* [#122](https://github.com/kowainik/co-log/issues/122):
  Add the `separate` combinator.
  (by [@vrom911](https://github.com/vrom911))
* [#125](https://github.com/kowainik/co-log/issues/125):
  Add monadic versions of contravariant functions.
  (by [@piq9117](https://github.com/piq9117))
* [#138](https://github.com/kowainik/co-log/issues/138):
  Add `hoistLogAction` — higher-order transformation function.
  (by [@jiribenes](https://github.com/jiribenes))
* [#123](https://github.com/kowainik/co-log/issues/123):
  Write default implementation to `getLogAction` via `logActionL`.
  (by [@SanchayanMaity](https://github.com/SanchayanMaity))

## 0.2.0.0 — May 5, 2019

* [#85](https://github.com/kowainik/co-log/issues/85):
  Move `overLogAction` to `HasLog` typeclass
* [#101](https://github.com/kowainik/co-log/issues/101):
  Add `logActionL` lens with default implementation to `HasLog` type class.
* [#99](https://github.com/kowainik/co-log/issues/99):
  Add comonadic combinators: `duplicate` and `multiplicate`.
* [#78](https://github.com/kowainik/co-log/issues/78):
  Improve documentation significantly.

## 0.1.1 — Nov 15, 2018

* [#63](https://github.com/kowainik/co-log/issues/63):
  Add `logPrint`, `logPrintStderr`, `logPrintHandle` and `withLogPrintFile` to `Colog.Core.IO`.
* [#46](https://github.com/kowainik/co-log/issues/46):
  Moves `logStringStdout`, `logStringStderr`, `logStringHandle`,
  `withLogStringFile` from `Colog.Actions` to `Colog.Core.IO`.
* [#48](https://github.com/kowainik/co-log/issues/48):
  Adds `liftLogIO` function.
* [#49](https://github.com/kowainik/co-log/issues/49):
  Add `<&` and `&>`operators for `unLogAction`.
* [#47](https://github.com/kowainik/co-log/issues/47):
  Add `doctest` tests.
* [#13](https://github.com/kowainik/co-log/issues/13):
  Add `.cabal` file description and improve documentation.
* [#39](https://github.com/kowainik/co-log/issues/39):
  Support GHC-8.2.2 and GHC-8.6.2.

## 0.1.0

* [#38](https://github.com/kowainik/co-log/issues/38):
  Rename `cbind` to `cmapM`.

* [#37](https://github.com/kowainik/co-log/issues/37):
  Add `base` bounds.

## 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/co-log/releases
