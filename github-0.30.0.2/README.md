GitHub
------

[![Hackage version](https://img.shields.io/hackage/v/github.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/github)
[![github on Stackage Nightly](https://stackage.org/package/github/badge/nightly)](https://stackage.org/nightly/package/github)
[![Stackage LTS version](https://www.stackage.org/package/github/badge/lts?label=Stackage)](https://www.stackage.org/package/github)
[![Haskell-CI](https://github.com/haskell-github/github/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/haskell-github/github/actions/workflows/haskell-ci.yml)

The GitHub API v3 for Haskell.

Some functions are missing; these are functions where the GitHub API did
not work as expected. The full GitHub API is in beta and constantly
improving.

Installation
============

In your project's cabal file:

```cabal
Build-depends:       github
```

Or from the command line:

```sh
cabal v1-install github
```

Example Usage
=============

See the samples in the
[samples/](https://github.com/haskell-github/github/tree/master/samples) directory.

Note: some samples might be outdated.

Documentation
=============

For details see the reference [documentation on Hackage][hackage].

Each module lines up with the hierarchy of
[documentation from the GitHub API](https://docs.github.com/en/rest).

Request functions (ending with `R`) construct a data type which can be executed
in `IO` by `executeRequest` functions. They are all listed in the root `GitHub`
module.

IO functions produce an `IO (Either Error a)`, where `a` is the actual thing
you want. You must call the function using IO goodness, then dispatch on the
possible error message. Here's an example from the samples:

Many function have samples under
[`samples/`](https://github.com/haskell-github/github/tree/master/samples) directory.

```hs
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude.Compat

import Data.Text         (Text, pack)
import Data.Text.IO as T (putStrLn)
import Data.Monoid       ((<>))

import GitHub (github')
import qualified GitHub

main :: IO ()
main = do
    possibleUsers <- github' GitHub.usersFollowingR "phadej"
    T.putStrLn $ either (("Error: " <>) . pack . show)
                        (foldMap ((<> "\n") . formatUser))
                        possibleUsers

formatUser :: GitHub.SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin
```

Contributions
=============

Please see
[CONTRIBUTING.md](https://github.com/haskell-github/github/blob/master/CONTRIBUTING.md)
for details on how you can help.

Copyright
=========

Copyright 2011-2012 Mike Burns.
Copyright 2013-2015 John Wiegley.
Copyright 2016-2019 Oleg Grenrus.

Available under the BSD 3-clause license.

[hackage]: https://hackage.haskell.org/package/github "Hackage"

Alternative
===========

Library [`github-rest`](https://hackage.haskell.org/package/github-rest)
also provides an interface to the GitHub API.
It compares itself to `github` here:
https://github.com/LeapYear/github-rest#comparison-to-other-libraries
