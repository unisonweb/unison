#!/usr/bin/env bash
# Regenerates unison-runtime/src/Unison/Runtime/Builtin/Decls.hs from the
# builtin declarations in parser-typechecker (GenBuiltinDecls.hs says why
# it is generated). Builds parser-typechecker first, so that the constants
# come from the current sources. Run from anywhere in the repository.
set -euo pipefail
cd "$(dirname "$0")/../.."
stack build --fast unison-parser-typechecker
stack exec -- ghc -package unison-parser-typechecker -package unison-core1 -package unison-hash -package text \
  -e main unison-runtime/scripts/GenBuiltinDecls.hs
# formatted as the formatting check wants it (development.md, "Formatting")
ormolu --mode inplace unison-runtime/src/Unison/Runtime/Builtin/Decls.hs
echo "wrote unison-runtime/src/Unison/Runtime/Builtin/Decls.hs"
