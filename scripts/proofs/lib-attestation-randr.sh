#!/usr/bin/env bash
# =============================================================================
# lib-attestation-randr.sh - Shared functions for attestation scripts
# =============================================================================
#
# This library provides common functions for the attestation scripts.
#
# USAGE:
#   source "$(dirname "$0")/lib-attestation-randr.sh"
#   attestation_setup "transcripts" "$@"
#   # ... tool-specific commands ...
#   attestation_record
#
# VARIABLES SET BY attestation_setup:
#   ATTESTATION_NAME      - Display name (e.g., "transcripts")
#   LOCAL_PROOFS_FILE     - Local cache file (e.g., ".transcripts-proofs-local")
#   TRACKED_PROOFS_FILE   - Tracked proofs file (e.g., ".github/workflows/proofs/transcripts.txt")
#   FORCE, VERBOSE, DRY_RUN, SUMMARY - Parsed from command line
#   HASH                  - Computed source hash
#   INFRA_HASH            - Computed infrastructure hash
#   SCRIPT_DIR            - Top-level scripts directory
#
# INFRASTRUCTURE HASH:
#   The infrastructure hash captures the run script contents.
#   If source matches but infrastructure differs, we skip with a warning suggesting
#   --force, since the way tests are invoked or hashed may have changed.
#
# =============================================================================

MAX_LOCAL_HASHES=100  # Cap for local file to prevent unbounded growth
ATTESTATION_PREREQ_EXIT_CODE=125

# Find the top-level scripts directory by walking up from this file
_find_scripts_dir() {
    local dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    while [[ "$dir" != "/" && "$(basename "$dir")" != "scripts" ]]; do
        dir="$(dirname "$dir")"
    done
    echo "$dir"
}

# -----------------------------------------------------------------------------
# attestation_setup <name> "$@"
# Set up attestation for the given name and parse command-line arguments
# -----------------------------------------------------------------------------
attestation_setup() {
    local name="$1"
    shift

    # Derive all paths from name
    ATTESTATION_NAME="$name"
    LOCAL_PROOFS_FILE=".local-proofs/${name}.txt"
    TRACKED_PROOFS_FILE=".github/workflows/proofs/${name}.txt"

    set -euo pipefail
    cd "$(git rev-parse --show-toplevel)"

    SCRIPT_DIR="$(_find_scripts_dir)"

    FORCE=false
    VERBOSE=false
    DRY_RUN=false
    SUMMARY=false
    for arg in "$@"; do
        case "$arg" in
            --force) FORCE=true ;;
            --verbose|-v) VERBOSE=true ;;
            --dry-run|-n) DRY_RUN=true ;;
            --summary) SUMMARY=true ;;
        esac
    done

    # Compute source hash by calling the run script with --hash
    local caller_script="${BASH_SOURCE[1]}"
    HASH=$("$caller_script" --hash)

    # Compute infrastructure hash (just the run script, which now contains PATTERNS)
    INFRA_HASH=$(cat "$caller_script" | shasum -a 256 | cut -d' ' -f1)

    if [[ "$VERBOSE" == "true" ]]; then
        echo "Source hash: $HASH"
        echo "Infra hash:  $INFRA_HASH"
    fi
}

# -----------------------------------------------------------------------------
# prereq_failed <message>...
# Exit without recording an attestation because a prerequisite was not met.
# -----------------------------------------------------------------------------
prereq_failed() {
    >&2 printf '%s\n' "$@"
    exit "$ATTESTATION_PREREQ_EXIT_CODE"
}

# -----------------------------------------------------------------------------
# attestation_require_commands <cmd>...
# Exit without recording an attestation if required commands are missing.
# -----------------------------------------------------------------------------
attestation_require_commands() {
    local missing=()
    local cmd
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            missing+=("$cmd")
        fi
    done

    if [[ ${#missing[@]} -eq 0 ]]; then
        return 0
    fi

    local plural="" noun="tool"
    if [[ ${#missing[@]} -ne 1 ]]; then
        plural="s"
        noun="tools"
    fi

    prereq_failed \
        "Cannot run $ATTESTATION_NAME: missing required $noun: ${missing[*]}" \
        "This is an environment/toolchain issue; no attestation was recorded."
}

# -----------------------------------------------------------------------------
# _lookup_attestation
# Look up hash in tracked file and local cache, set result variables
# Sets: CACHED_RESULT (pass|fail|none), CACHED_SOURCE (tracked|local|none),
#       CACHED_INFRA, INFRA_CHANGED
# -----------------------------------------------------------------------------
_lookup_attestation() {
    CACHED_RESULT=none
    CACHED_SOURCE=none
    CACHED_INFRA=""
    INFRA_CHANGED=false

    # Check tracked file first
    if [[ -f "$TRACKED_PROOFS_FILE" ]]; then
        local line=$(grep "^$HASH " "$TRACKED_PROOFS_FILE" 2>/dev/null || true)
        local result=$(echo "$line" | awk '{print $3}')
        if [[ "$result" == "pass" || "$result" == "fail" ]]; then
            CACHED_RESULT="$result"
            CACHED_SOURCE=tracked
            CACHED_INFRA=$(echo "$line" | awk '{print $2}')
            if [[ -n "$CACHED_INFRA" && "$CACHED_INFRA" != "$INFRA_HASH" ]]; then
                INFRA_CHANGED=true
            fi
            return
        fi
    fi

    # Check local cache
    if [[ -f "$LOCAL_PROOFS_FILE" ]]; then
        local line=$(grep " $HASH " "$LOCAL_PROOFS_FILE" 2>/dev/null | tail -1 || true)
        local result=$(echo "$line" | awk '{print $4}')
        if [[ "$result" == "pass" || "$result" == "fail" ]]; then
            CACHED_RESULT="$result"
            CACHED_SOURCE=local
            CACHED_INFRA=$(echo "$line" | awk '{print $3}')
            if [[ -n "$CACHED_INFRA" && "$CACHED_INFRA" != "$INFRA_HASH" ]]; then
                INFRA_CHANGED=true
            fi
            return
        fi
    fi
}

# -----------------------------------------------------------------------------
# attestation_check_skip
# Check if we can skip based on existing attestations
# Exits 0 if cached pass, exits 1 if cached fail, returns if should run
# -----------------------------------------------------------------------------
attestation_check_skip() {
    # Lookup cached result (unless forcing)
    if [[ "$FORCE" != "true" ]]; then
        _lookup_attestation
    else
        CACHED_RESULT=none
        INFRA_CHANGED=false
    fi

    # Determine what to do and what message to show
    local action summary
    case "$CACHED_RESULT" in
        fail)
            action=exit_fail
            if [[ "$INFRA_CHANGED" == "true" ]]; then
                summary="cached (fail, infra changed, consider --force)"
            else
                summary="cached (fail)"
            fi
            ;;
        pass)
            action=exit_pass
            if [[ "$INFRA_CHANGED" == "true" ]]; then
                summary="cached (pass, infra changed, consider --force)"
            else
                summary="cached (pass)"
            fi
            ;;
        *)
            if [[ "$DRY_RUN" == "true" ]]; then
                action=dry_run
                summary="would run"
            else
                action=run
                summary="running"
            fi
            ;;
    esac

    # Output
    if [[ "$SUMMARY" == "true" ]]; then
        echo "$summary"
    else
        case "$action" in
            exit_fail)
                echo "Attestation found: $ATTESTATION_NAME previously FAILED."
                echo "  Fix the issues and run again, or use --force to re-run."
                ;;
            exit_pass)
                echo "Attestation found, skipping $ATTESTATION_NAME. (Use --force to run anyway.)"
                ;;
            dry_run)
                echo "Attestation not found, would run $ATTESTATION_NAME."
                ;;
            run)
                echo "Attestation not found, running $ATTESTATION_NAME."
                echo ""
                ;;
        esac
        if [[ "$INFRA_CHANGED" == "true" ]]; then
            echo "  Warning: Infrastructure changed since attestation was recorded."
            echo "           Consider running with --force."
        fi
    fi

    # Verbose details
    if [[ "$VERBOSE" == "true" ]]; then
        case "$CACHED_SOURCE" in
            tracked) echo "  Found in: $TRACKED_PROOFS_FILE (shared, tracked by git)" ;;
            local)   echo "  Found in: $LOCAL_PROOFS_FILE (local cache, gitignored)" ;;
            *)       echo "  Not in: $TRACKED_PROOFS_FILE or $LOCAL_PROOFS_FILE" ;;
        esac
        if [[ "$INFRA_CHANGED" == "true" ]]; then
            echo "  Recorded infra: $CACHED_INFRA"
            echo "  Current infra:  $INFRA_HASH"
        fi
    fi

    # Side effects and exit
    case "$action" in
        exit_fail|exit_pass)
            if [[ "$CACHED_SOURCE" == "local" && "$DRY_RUN" != "true" ]]; then
                mkdir -p "$(dirname "$TRACKED_PROOFS_FILE")"
                echo "$HASH $INFRA_HASH $CACHED_RESULT" > "$TRACKED_PROOFS_FILE"
            fi
            [[ "$CACHED_RESULT" == "pass" ]] && exit 0 || exit 1
            ;;
        dry_run|run)
            return 0
            ;;
    esac
}

# -----------------------------------------------------------------------------
# attestation_record_and_exit <exit_code>
# Record pass/fail based on exit code, then exit with appropriate code
# -----------------------------------------------------------------------------
attestation_record_and_exit() {
    local exit_code="$1"
    if [[ $exit_code -eq 0 ]]; then
        attestation_record pass
    elif [[ $exit_code -eq $ATTESTATION_PREREQ_EXIT_CODE ]]; then
        exit "$exit_code"
    else
        attestation_record fail
        exit 1
    fi
}

# -----------------------------------------------------------------------------
# attestation_record [pass|fail]
# Record hash to local cache and tracked file after run
# Format: "timestamp source_hash infra_hash pass|fail" (local)
#         "source_hash infra_hash pass|fail" (tracked)
# Default result is "pass" if not specified
# -----------------------------------------------------------------------------
attestation_record() {
    local result="${1:-pass}"

    # Record to local file (append with cap, sorted by timestamp to keep newest)
    mkdir -p "$(dirname "$LOCAL_PROOFS_FILE")"
    local timestamp=$(date +%s)
    {
        if [[ -f "$LOCAL_PROOFS_FILE" ]]; then
            cat "$LOCAL_PROOFS_FILE"
        fi
        echo "$timestamp $HASH $INFRA_HASH $result"
    } | sort -n | tail -n "$MAX_LOCAL_HASHES" > "$LOCAL_PROOFS_FILE.tmp"
    mv "$LOCAL_PROOFS_FILE.tmp" "$LOCAL_PROOFS_FILE"

    [[ "$VERBOSE" == "true" ]] && echo "Recorded to $LOCAL_PROOFS_FILE"

    # Update tracked proofs file
    mkdir -p "$(dirname "$TRACKED_PROOFS_FILE")"
    echo "$HASH $INFRA_HASH $result" > "$TRACKED_PROOFS_FILE"
    if [[ "$VERBOSE" == "true" ]]; then
        echo "Tracked file: $TRACKED_PROOFS_FILE"
        echo "  Updated with current hashes and result=$result"
    fi

    # In summary mode, skip the result message (test output speaks for itself)
    if [[ "$SUMMARY" == "true" ]]; then
        return 0
    fi

    echo ""
    echo "Done. Proofs file updated ($result)."

    # Remind user to commit proofs file (unless our hook will handle it)
    local git_common_dir=$(git rev-parse --git-common-dir 2>/dev/null || git rev-parse --git-dir)
    local installed_hook="$git_common_dir/hooks/pre-push"
    local source_hook="$SCRIPT_DIR/hooks/pre-push.bash"
    local installed_hash=$(shasum "$installed_hook" 2>/dev/null | cut -d' ' -f1)
    local source_hash=$(shasum "$source_hook" 2>/dev/null | cut -d' ' -f1)

    if [[ -n "$source_hash" && "$installed_hash" == "$source_hash" ]]; then
        return 0  # Our hook is installed, it will handle things
    fi

    echo ""
    echo "Commit the proofs file with your changes, then push:"
    echo "  git add $TRACKED_PROOFS_FILE && git commit"
    if [[ ! -x "$installed_hook" ]]; then
        echo ""
        echo "(Or install the pre-push hook to automate this: ./scripts/hooks/install.bash)"
    fi
}
