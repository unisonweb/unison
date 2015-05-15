#!/bin/sh
cabal sandbox init
cd shared
cabal sandbox init --sandbox ../.cabal-sandbox
cd ../node
cabal sandbox init --sandbox ../.cabal-sandbox
cabal sandbox add-source ../shared
cabal install --only-dependencies
cabal install --only-dependencies --enable-tests
cabal build
