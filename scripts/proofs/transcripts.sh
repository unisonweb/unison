#!/usr/bin/env bash
# =============================================================================
# transcripts.sh - Run transcripts and record attestation
# =============================================================================
#
# USAGE:
#   ./scripts/proofs/transcripts.sh [OPTIONS] [COMMIT]
#
# OPTIONS:
#   --hash          Compute and print hash only (no run, no record)
#   --force         Force re-run even if attestation exists
#   --verbose|-v    Verbose output
#   --dry-run|-n    Show what would happen without running
#   --summary       Summary mode for pre-push hook integration
#
# ARGUMENTS:
#   COMMIT          Optional commit SHA to hash (only valid with --hash)
#
# See lib-attestation-randr.sh for details on the attestation system.
#
# =============================================================================

# FILES INCLUDED IN HASH:
PATTERNS=(
    # Build configuration
    'stack.yaml'
    '**/package.yaml'
    # Haskell source (affects UCM behavior)
    '**/*.hs'
    # Transcript inputs (need both *.ext and **/*.ext because ** doesn't match zero dirs)
    'unison-src/*.md'
    'unison-src/**/*.md'
    'unison-src/*.u'
    'unison-src/**/*.u'
    # This script (for hash integrity)
    '**/transcripts.sh'
)

# Handle --hash and --check early, before sourcing other libraries
source "$(dirname "$0")/lib-attestation-hash.sh"
handle_flags "$@"

source "$(dirname "$0")/lib-attestation-randr.sh"
attestation_setup "transcripts" "$@"
attestation_check_skip
attestation_require_commands stack

# === Tool-specific commands ===
# Run in subshell to capture exit status while allowing errexit inside
set +e
(
    set -euo pipefail

    echo "Building ucm and transcripts binaries..."
    stack build --fast unison-cli:exe:transcripts unison-cli-main:exe:unison

    UCM=$(stack exec -- which unison)
    TRANSCRIPTS=$(stack exec -- which transcripts)

    echo ""
    echo "Running round-trip tests..."
    "$UCM" transcript unison-src/transcripts-round-trip/main.md
    "$UCM" transcript unison-src/transcripts-manual/rewrites.md

    echo "Checking for round-trip diffs..."
    git diff --ignore-cr-at-eol --exit-code \
        unison-src/transcripts-round-trip/main.output.md \
        unison-src/transcripts-manual/rewrites.output.md

    echo ""
    echo "Running main transcripts..."
    "$TRANSCRIPTS"

    echo "Checking for transcript diffs..."
    git diff --ignore-cr-at-eol --exit-code unison-src/transcripts

    echo ""
    echo "Running docs.to-html transcript..."
    "$UCM" transcript unison-src/transcripts-manual/docs.to-html.md

    echo "Checking for docs diffs..."
    git diff --ignore-cr-at-eol --exit-code \
        unison-src/transcripts-manual/docs.to-html.output.md \
        unison-src/transcripts-manual/docs.to-html

    echo ""
    echo "Running fix5507 regression test..."
    "$UCM" transcript unison-src/tests/fix5507.md
    "$UCM" run.compiled fix5507.uc
)
attestation_record_and_exit $?
