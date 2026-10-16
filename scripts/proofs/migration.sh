#!/usr/bin/env bash
# =============================================================================
# migration.sh - Check that this ucm migrates the latest release's codebase,
# and record attestation
# =============================================================================
#
# USAGE:
#   ./scripts/proofs/migration.sh [OPTIONS] [COMMIT]
#
# OPTIONS:
#   --hash          Compute and print hash only (no run, no record)
#   --check         Print the attestation status for the current hash
#   --force         Force re-run even if attestation exists
#   --verbose|-v    Verbose output
#   --dry-run|-n    Show what would happen without running
#   --summary       Summary mode for pre-push hook integration
#
# ARGUMENTS:
#   COMMIT          Optional commit SHA to hash (only valid with --hash)
#
# WHAT IT PROVES:
#   That the development ucm can open and migrate a codebase created by the
#   latest numbered release (scripts/migration-test.sh). The hash covers the
#   migration code, the schema version, the test script, and the tag of the
#   latest release, recorded in .github/workflows/proofs/migration-release.txt.
#   A real run refreshes that file first, so the attestation is always for the
#   release that is latest at the time of the run; when a new release comes
#   out the hash changes and the check has to be run again. CI verifies both
#   the hash and that the recorded tag is still the latest.
#
# See lib-attestation-randr.sh for details on the attestation system.
#
# =============================================================================

RELEASE_FILE=.github/workflows/proofs/migration-release.txt

# FILES INCLUDED IN HASH:
PATTERNS=(
    # The schema version and the migrations
    'codebase2/codebase-sqlite/U/Codebase/Sqlite/Queries.hs'
    '**/SqliteCodebase.hs'
    '**/SqliteCodebase/Migrations.hs'
    '**/SqliteCodebase/Migrations/**'
    # The test and this script
    '**/migration-test.sh'
    '**/proofs/migration.sh'
    # The release the test was run against
    '.github/workflows/proofs/migration-release.txt'
)

# A real run (no --hash/--check/--list-files) records the latest release's
# tag before the hash is computed, so that the hash covers it.
case "${1:-}" in
    --hash|--check|--list-files) ;;
    *)
        cd "$(git rev-parse --show-toplevel)"
        if ! latest=$(./scripts/migration-test.sh --latest 2>/dev/null); then
            >&2 echo "Cannot run migration: could not find the latest release (no network?)."
            >&2 echo "This is an environment issue; no attestation was recorded."
            exit 125
        fi
        mkdir -p "$(dirname "$RELEASE_FILE")"
        if [[ ! -f "$RELEASE_FILE" || "$(cat "$RELEASE_FILE")" != "$latest" ]]; then
            echo "$latest" > "$RELEASE_FILE"
        fi
        ;;
esac

# Handle --hash and --check early, before sourcing other libraries
source "$(dirname "$0")/lib-attestation-hash.sh"
handle_flags "$@"

source "$(dirname "$0")/lib-attestation-randr.sh"
attestation_setup "migration" "$@"
attestation_check_skip
attestation_require_commands curl tar

# === Tool-specific commands ===
set +e
(
    set -euo pipefail
    echo "Checking that this ucm migrates a codebase made by $(cat "$RELEASE_FILE")..."
    ./scripts/migration-test.sh
)
attestation_record_and_exit $?
