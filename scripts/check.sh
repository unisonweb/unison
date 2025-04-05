#!/bin/bash

# eventually: ormolu -c `find . -name '*.hs'`
true \
  && stack build --fast --test \
  && stack exec transcripts \
  && stack exec unison transcript unison-src/transcripts-round-trip/main.md \
  && stack exec unison transcript unison-src/transcripts-manual/rewrites.md \
  && stack exec unison transcript unison-src/transcripts-manual/docs.to-html.md \
  && stack exec cli-integration-tests
