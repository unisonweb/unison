#!/usr/bin/env bash
set -euo pipefail
SCRIPTDIR="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

## TODO: `transcripts` should be able to take a search path, but since it currently looks for files relative to its run
##       path, we `cd` to where it can find them.
cd "$SCRIPTDIR/.."

true \
  && stack build --fast --test \
  && stack exec transcripts \
  && stack exec unison transcript unison-src/transcripts-round-trip/main.md \
  && stack exec unison transcript unison-src/transcripts-manual/rewrites.md \
  && stack exec unison transcript unison-src/transcripts-manual/docs.to-html.md \
  && stack exec cli-integration-tests \
  && ./scripts/check-weeds \
  && ./scripts/check-formatting
