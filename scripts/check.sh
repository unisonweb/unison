#!/usr/bin/env bash
# =============================================================================
# check.sh - Run all checks before pushing
# =============================================================================
#
# USAGE:
#   ./scripts/check.sh [--force] [--verbose|-v] [--dry-run|-n]
#
# This script runs all the checks needed before pushing:
#   1. Unit tests and integration tests (with proof caching)
#   2. Transcript tests (with proof caching)
#   3. Formatting check
#
# All checks with proof caching will:
#   - Skip if already passed for current codebase state
#   - Update tracked proofs files for CI verification
#   - Allow CI to skip re-running when proofs are present
#
# See scripts/proofs/{tests,transcripts,weeds}.sh for details.
#
# =============================================================================

set -euo pipefail
SCRIPTDIR="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Parse arguments
FORCE=false
VERBOSE=false
DRY_RUN=false
for arg in "$@"; do
    case "$arg" in
        --force) FORCE=true ;;
        --verbose|-v) VERBOSE=true ;;
        --dry-run|-n) DRY_RUN=true ;;
    esac
done

cd "$SCRIPTDIR/.."

# Build args for sub-scripts
ARGS=("--summary")
[[ "$FORCE" == "true" ]] && ARGS+=("--force")
[[ "$VERBOSE" == "true" ]] && ARGS+=("--verbose")
[[ "$DRY_RUN" == "true" ]] && ARGS+=("--dry-run")

# Run a check with compact output
# Usage: run_check "Label" script [args...]
run_check() {
    local label="$1"
    shift
    printf "%-16s" "$label"
    "$@"
}

run_check "Tests:"        ./scripts/proofs/tests.sh "${ARGS[@]}"
run_check "Transcripts:"  ./scripts/proofs/transcripts.sh "${ARGS[@]}"
run_check "Formatting:"   ./scripts/proofs/formatting.sh "${ARGS[@]}"
# run_check "Dead code:"    ./scripts/proofs/weeds.sh "${ARGS[@]}"

echo ""
echo "All checks passed!"
