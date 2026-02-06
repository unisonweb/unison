#!/usr/bin/env bash
# =============================================================================
# formatting.sh - Run formatting check and record attestation
# =============================================================================
#
# USAGE:
#   ./scripts/proofs/formatting.sh [OPTIONS] [COMMIT]
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
    # Ormolu configuration
    '.ormolu'
    # Haskell source (files being formatted)
    '**/*.hs'
    # Formatting check script
    'scripts/check-formatting'
    # This script (for hash integrity)
    '**/formatting.sh'
)

# Handle --hash early, before sourcing other libraries
if [[ "${1:-}" == "--hash" ]]; then
    source "$(dirname "$0")/lib-attestation-hash.sh"
    compute_hash "${2:-}"
    exit 0
fi

source "$(dirname "$0")/lib-attestation-randr.sh"
attestation_setup "formatting" "$@"
attestation_check_skip

# === Tool-specific commands ===
# Run in subshell to capture exit status while allowing errexit inside
set +e
(
    set -euo pipefail

    echo "Checking code formatting with ormolu..."
    "$SCRIPT_DIR/check-formatting"
)
attestation_record_and_exit $?
