Changelog for filelock package

0.1.1.9
-------

_2026-02-02, Andreas Abel_

* Bump `cabal-version` to 2.2 to get license identifier to `CC0-1.0`
  ([Issue #10](https://github.com/haskell-pkg-janitors/filelock/issues/10)).
* Drop support for GHCs 8.0 and 8.2.

Tested with GHC 8.4 - 9.14.1.

0.1.1.8
-------

_2025-08-27, Andreas Abel_

* Remove obsolete `deriving Typeable`.

Tested with GHC 8.0 - 9.14.1-alpha1.

0.1.1.7
-------

_2023-08-03, Andreas Abel_

* Add `build-tools: hsc2hs` to `.cabal` for building with GHC 8.x

Tested with GHC 8.0 - 9.8.1-alpha1.

0.1.1.6
-------

_2023-04-06, Andreas Abel_

* Fix problem with locking when used with `unix-2.8`
  (issue [#12](https://github.com/takano-akio/filelock/issues/12)).
* Package moved to [@haskell-pkg-janitors](https://github.com/haskell-pkg-janitors/filelock).

Tested with GHC 8.2 - 9.6.

0.1.1.5
-------

_2020-06-30, Takano Akio_
