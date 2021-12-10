#!/bin/bash

# eventually: ormolu -c `find . -name '*.hs'`
true \
  && stack build --fast \
  && stack test unison-cli \
  && stack exec tests \
  && stack test unison-util-relation \
  && stack exec transcripts \
  && stack exec unison transcript unison-src/transcripts-round-trip/main.md \
  && stack exec integration-tests
