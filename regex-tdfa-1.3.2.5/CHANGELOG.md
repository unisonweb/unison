### 1.3.2.5

_2025-09-29 Andreas Abel_

- Bump `cabal-version` to 2.0 to support field `autogen-modules` ([#70](https://github.com/haskell-hvr/regex-tdfa/pull/70))
- SPECIALIZE pragmas for faster `Text` matching ([#72](https://github.com/haskell-hvr/regex-tdfa/pull/72))
- Address deprecation warning for `sizeofMutualByteArray#` ([#71](https://github.com/haskell-hvr/regex-tdfa/issues/71))
- Tested with GHC 8.0 - 9.12.2

### 1.3.2.4

_2025-05-09 Andreas Abel_

- Fix definition of `[[:graph:]]` character class ([#66](https://github.com/haskell-hvr/regex-tdfa/issues/66))
- Tested with GHC 8.0 - 9.12.2

### 1.3.2.3

_2025-03-02 Andreas Abel_

- Drop support for GHC 7
- Allow `containers < 1`
- Tested with GHC 8.0 - 9.12.1

### 1.3.2.2

_2023-08-02, Andreas Abel_

- Fix return type in `memcpy` FFI signature ([#52](https://github.com/haskell-hvr/regex-tdfa/pull/52))
- Refactor `regexec` to avoid partial functions `tail` and `(!0)`
- Tested with GHC 7.4 - 9.8.1-alpha1

### 1.3.2.1

_2023-05-19, Andreas Abel_

- Fix haddock rendering of code examples in top-level documentation
  ([#50](https://github.com/haskell-hvr/regex-tdfa/issues/50))
- Tested with GHC 7.4 - 9.6

## 1.3.2

_2022-07-18, Andreas Abel_

- Export `decodePatternSet` and `decodeCharacterClass` from `Text.Regex.TDFA.Pattern`
  ([#16](https://github.com/haskell-hvr/regex-tdfa/issues/16))
- Extend and correct docs for `Pattern` module
- Tested with GHC 7.4 - 9.4

### 1.3.1.5

_2022-07-18, Andreas Abel_

- Allow dash (`-`) as start of a range, e.g. `[--z]`
  ([#1](https://github.com/haskell-hvr/regex-tdfa/issues/1),
  [#45](https://github.com/haskell-hvr/regex-tdfa/pull/45))
- Tested with GHC 7.4 - 9.4

### 1.3.1.4

_2022-07-17, Andreas Abel_

- Fix parsing of dashes in bracket expressions, e.g. `[-a-z]` ([#1](https://github.com/haskell-hvr/regex-tdfa/issues/1))
- Fix a deprecation warning except for on GHC 8.2 ([#21](https://github.com/haskell-hvr/regex-tdfa/issues/21))
- Documentation: link `defaultComptOpt` to its definition  ([#13](https://github.com/haskell-hvr/regex-tdfa/issues/13))
- Verify documentation examples with new `doc-test` testsuite
- Tested with GHC 7.4 - 9.4

### 1.3.1.3

_2022-07-14, Andreas Abel_

- Fix an `undefined` in `Show PatternSet` ([#37](https://github.com/haskell-hvr/regex-tdfa/issues/37))
- Document POSIX character classes (e.g. `[[:digit:]]`) in README
- Tested with GHC 7.4 - 9.4

### 1.3.1.2 Revision 1

_2022-05-25, Andreas Abel_

- Allow `base >= 4.17` (GHC 9.4)

### 1.3.1.2

_2022-02-19, Andreas Abel_
- No longer rely on the `MonadFail` instance for `ST`
  (future `base` library change, see [#29](https://github.com/haskell-hvr/regex-tdfa/pull/29)).
- Silence warning `incomplete-uni-patterns` (GHC >= 9.2).
- Import from `Data.List` explicitly or qualified (warning `compat-unqualified-imports`).
- Import from `Control.Monad` to allow `mtl-2.3` in its `rc3` incarnation.

### 1.3.1.1 Revision 3

_2022-01-31, Andreas Abel_
- Speculatively allow unreleased `mtl-2.3` (works with release candidate `mtl-2.3-rc4`).

### 1.3.1.1 Revision 2

_2021-12-26, Andreas Abel_
- Allow `text-2.0`.

### 1.3.1.1 Revision 1

_2021-08-12, Andreas Abel_
- Compatibility with `base-4.16` (GHC 9.2).

### 1.3.1.1

_2021-06-03, Andreas Abel_
- Removed extension `NoMonoPatBinds` from `.cabal`-file for GHC 9.2 compatibility.
- Removed some outdated documentation.

### 1.3.1.0 Revision 2

_2021-02-20, Andreas Abel_
- Compatibility with `base-4.15` (GHC 9.0) and `bytestring-0.11`.

### 1.3.1.0 Revision 1

_2020-03-26, phadej_
- Compatibility with `base-4.14` (GHC 8.10).

## 1.3.1.0

_2019-11-26, hvr_
- Merge <http://hackage.haskell.org/package/regex-tdfa-text> into `regex-tdfa`; see <https://github.com/haskell-hvr/regex-tdfa/issues/4>.
- Don't inject `ghc-options: -O2` by default anymore (see #7 for rationale) and introduce `force-O2` cabal flag to control the injection of `ghc-options: -O2`.
  Note that you can conveniently control optimization levels on a per-package granularity via `cabal.project` files; see [cabal's user-guide](https://cabal.readthedocs.io/en/latest/nix-local-build.html#configuring-builds-with-cabal-project) for more details.

## 1.3.0 Revision 1

_2019-11-26, hvr_
- Tighten bounds on `base`, `mtl`, `parsec` and fail.

# 1.3.0

_2019-09-29, Artyom_
- Same as 1.2.3.3 release, but in accordance to PVP;
  see <https://github.com/ChrisKuklewicz/regex-tdfa/issues/29>.
- Compatibility with GHC 8.8 and regex-base-0.9.4 (h/t @asr).
- Turned `regex-tdfa-unittest` into a `regex-tdfa` testsuite.

### 1.2.3.3 (deprecated, not following PVP)

* Compatibility with GHC 8.8 and regex-base-0.9.4 (h/t @asr).
* Turned `regex-tdfa-unittest` into a `regex-tdfa` testsuite.

### 1.2.3.2

_2019-05-09, Artyom_
* Significantly improved documentation (h/t William Yao).

### 1.2.3.1

_2018-06-22, Artyom_
* Compatibility with `containers-0.6`.

## 1.2.3

_2018-03-10, Artyom_
* Added `Semigroup` instances for some types (h/t Herbert Valerio Riedel).

## 1.2.2

_2016-04-28, Artyom_
* New maintainer.
* Now we don't reexport the problematic `Show` instance for functions.

## 1.2.1

_2015-08-29, Chris Kuklewicz_
* Updated dependency versions.

# 1.2.0

_2014-02-02, Chris Kuklewicz_
* "Almost ghc-7.8" with the array 0.4 changes for `Data.Array.Unsafe`


## 1.1.8

Make ghc-7.0.2 on platform 2011.2.0.0.0 happy

## 1.1.7

fix url below

## 1.1.6

Fix bug preventing `[]] [-] [^]] [^-]` (thanks to Maxime Henrion)

## 1.1.5

try `needUniqTags` in `POr` in CorePattern.hs, try `toAdvice b` for `PStar child`

## 1.1.4

fixed

## 1.1.3

BROKEN after 100 characters the `compressOrbit` dies!

## 1.1.2

worked

## 1.1.1

add gnu escapes

# 1.1.0

NewDFA code working

## 1.0.7

make NewDFA directory and String_NC

## 1.0.6

try NewDFATest_SBS with `uncons`

## 1.0.5

use `uncons` on SBS

## 1.0.4

try repaired NewDFATest_SBS

* np13: try to improve readability with the `mm` combinator? Yes!
* np12: expand `o` in the case where `t` lookup get `Nothing`? Yes – this is the fix!?
* np11: break multi to not look at `o` and just return `True`? Yes !!!!
* np10: Peel off `CharMap`/`IntMap` and DFA/DT with pattern matching? No
* np9:  `INLINE` `endOf`? No
* np8:  np6 and `NOINLINE` `endOff`? No
* np7:  just return `True`? Fast
* np6:  comment out ans check? No
* np5:  comment out all `Multi0` code? No
* np4:  comment out all `Single0` and `Single` code? No
* np3:  `!off` the multi? No
* np2:  comment out all Testing code? No

## 1.0.3

try to alter `matchTest` to not have the `Bool` args? No

## 1.0.2

arg, the prof is fast and the normal slow!

# 1.0.1

add NewDFATest.hs

## 0.99.20

go to many vs single?

## 0.99.19

try for pre-comparison of orbit-logs!

## 0.99.18

try alternate lazy/strict strategy in NewDFA. Fix offset laziness.

## 0.99.17

radical removal of flag array and adding of `SetVal` to handle groups

## 0.99.16

performance? up to v15

## 0.99.15

get string with NewDFA testing, unit tests and 1000 random regex pass

## 0.99.14

start changing to the new real DFA

## 0.99.13

more cleanup

## 0.99.12

try to debug 0.99.11: fixed `updateWinner`

## 0.99.11

improve above fix and make stuff work better – HAS BUG, along with old TDFA!

## 0.99.10

fixed `((.?)*)*` patterns by changing `PStar nullView` when `mayFirstBeNull`

## 0.99.9

testing changing `bestTrans`/`chooseWith`/`choose` to include `enterOrbit`/`newFlags`/`(_,True)` info

## 0.99.8

testing changing `Maximize` to `Minimize` for `Tag`s, decide `(a*)*` is canonical problem

## 0.99.7

Use `(PGroup Nothing)` in `Pattern` to decompose `PBound`

## 0.99.6

change to nested `nonEmpty` calls for `PBound`

## 0.99.5

remove `PNonEmpty` constructor

## 0.99.4

tests `pnonempty' = \ p -> POr [ PEmpty, p ]` instead of `PNonEmpty`
