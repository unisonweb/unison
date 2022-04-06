#!/bin/bash

# eventually: ormolu -c `find . -name '*.hs'`
true \
  && stack build --fast --test \
  && stack exec transcripts \
  && stack exec unison transcript unison-src/transcripts-round-trip/main.md \
  && stack exec integration-tests
