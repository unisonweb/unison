#!/usr/bin/env bash
# =============================================================================
# tests.sh - Run tests and record attestation
# =============================================================================
#
# USAGE:
#   ./scripts/proofs/tests.sh [OPTIONS] [COMMIT]
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
    # Haskell source (affects test behavior)
    '**/*.hs'
    # Integration test inputs
    'unison-cli-integration/integration-tests/**/*.md'
    'unison-cli-integration/integration-tests/**/*.u'
    # This script (for hash integrity)
    '**/tests.sh'
    # diff3 test inputs
    '**/unison-util-diff3/**/testcases/**'
)

# Handle --hash and --check early, before sourcing other libraries
source "$(dirname "$0")/lib-attestation-hash.sh"
handle_flags "$@"

source "$(dirname "$0")/lib-attestation-randr.sh"
attestation_setup "tests" "$@"
attestation_check_skip

# === Tool-specific commands ===
# Run in subshell to capture exit status while allowing errexit inside
set +e
(
    set -euo pipefail

    echo "Building and running unit tests..."
    stack build --fast --test

    echo ""
    echo "Running CLI integration tests..."
    stack exec cli-integration-tests
)
attestation_record_and_exit $?
