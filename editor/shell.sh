#!/bin/sh
nix-shell --option extra-binary-caches https://ryantrinkle.com:5443/ -j 8 -A ghcjs
