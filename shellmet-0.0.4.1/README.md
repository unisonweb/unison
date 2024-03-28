# shellmet

[![GitHub CI](https://github.com/kowainik/shellmet/workflows/CI/badge.svg)](https://github.com/kowainik/shellmet/actions)
[![Hackage](https://img.shields.io/hackage/v/shellmet.svg?logo=haskell)](https://hackage.haskell.org/package/shellmet)
[![Stackage Lts](http://stackage.org/package/shellmet/badge/lts)](http://stackage.org/lts/package/shellmet)
[![Stackage Nightly](http://stackage.org/package/shellmet/badge/nightly)](http://stackage.org/nightly/package/shellmet)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Out of the shell solution for scripting in Haskell. Shellmet provides an easy and
convenient way to call shell commands from Haskell programs.

## Usage example

This README contains the usage example of the `shellmet` library. The example is
runnable. You can build and execute with the following command:

```shell
cabal run readme
```

### Setting up

Since this tutorial is written using Literate Haskell, first, let's write all
necessary pragmas and imports.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))

import Shellmet (($|))

import qualified Data.Text as T
```

### Simple scripting example

Below you can see how easy it is to interact with shell commands in Haskell:

```haskell
main :: IO ()
main = do
    "echo" ["Starting shellmet readme..."]
    text <- "cat" $| ["README.md"]
    let cnt = T.pack $ show $ length $ T.lines text
    "echo" ["Number of lines in this README: " <> cnt]
```

And the output is:

```
⚙  echo 'Starting shellmet readme...'
Starting shellmet readme...
⚙  echo 'Number of lines in this README: 54'
Number of lines in this README: 54
```
