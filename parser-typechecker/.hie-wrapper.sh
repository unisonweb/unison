#!/usr/bin/env bash

nix-shell --run "hie-8.4.4 -l hie.log -d --vomit $@ 2> hie.err"
