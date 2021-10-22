#!/bin/bash

# Early exit on failure
set -e

stack haddock --fast
docs_root="$(stack path --local-doc-root)"

git checkout --force haddocks
rm -r ./*
cp -r "${docs_root}"/* ./
git add .
git commit -m "Regenerate haddocks based on ${GITHUB_SHA}"
git push
