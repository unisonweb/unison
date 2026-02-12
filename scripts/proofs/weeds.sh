#!/usr/bin/env bash
# =============================================================================
# weeds.sh - Run dead code analysis and record attestation
# =============================================================================
#
# USAGE:
#   ./scripts/proofs/weeds.sh [OPTIONS] [COMMIT]
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
    # Weeder configuration
    'weeder.toml'
    # Haskell source (affects dead code analysis)
    '**/*.hs'
    # Weeds checking script
    'scripts/check-weeds'
    # This script (for hash integrity)
    '**/weeds.sh'
)

# Handle --hash and --check early, before sourcing other libraries
source "$(dirname "$0")/lib-attestation-hash.sh"
handle_flags "$@"

source "$(dirname "$0")/lib-attestation-randr.sh"
attestation_setup "weeds" "$@"
attestation_check_skip

# === Tool-specific commands ===
# Run in subshell to capture exit status while allowing errexit inside
set +e
(
    set -euo pipefail

    echo "Building with HIE files..."
    stack build --fast

    echo ""
    echo "Running dead code analysis (weeds)..."
    "$SCRIPT_DIR/check-weeds"
)
attestation_record_and_exit $?
