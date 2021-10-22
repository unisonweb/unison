#!/bin/bash

# Early exit on failure
set -e

stack haddock --fast
docs_root="$(stack path --local-doc-root)"

# Since the docs_root is a hidden & gitignored directory, these steps don't wipe it out.
git checkout --force haddocks
# Clear all current docs.
rm -r ./*
# Replace with new docs
cp -r "${docs_root}"/* ./

git add .
git commit -m "Regenerate haddocks based on ${GITHUB_SHA}"
git push
