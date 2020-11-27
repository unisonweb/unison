#!/bin/sh

MODE="inplace"
if [[ "$1" == "check" ]]; then
  MODE="check"
fi

ormolu --mode "$MODE" $(find . -name '*.hs' -not -path '*/.stack-work/*')
