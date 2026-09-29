![GitHub Actions status](https://github.com/haskell/network/workflows/Haskell%20CI/badge.svg)
# [`network`](http://hackage.haskell.org/package/network)

To build this package directly from git, you must run `autoreconf -i`.
And then use `cabal configure; cabal build` or `stack build`.

## Support Policy

### GHC

The `network` package support [3 major versions of GHC](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/intro.html#ghc-version-numbering-policy) only.
This means that the current stable version and two previous stable versions are supported.

### Windows

We use MSYS to build this package on Windows.
To use the `network` package on Cygwin, use `stack`.

## Coding

### .hs files

If you need C macros created by "configure" or `CALLCONV`/`SAFE_ON_WIN`, put

```
#include "HsNetDef.h"
```

"HsNet.h" does now work well since Mac's cpp sucks.

### .hsc files

If you need `#peek`, `#poke` and others, create a `.hsc` file with

```
#include "HsNet.h"
```

`HsNet.h` includes `HsNefDef.h` and necessary C structures.
Unfortunately, `hsc2hs` does not convert C macros.
So, if you use `CALLCONV`/`SAFE_ON_WIN`, the following is also necessary:

```
##include "HsNetDef.h"
```
