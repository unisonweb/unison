#!/usr/bin/env bash
# =============================================================================
# lib-attestation-hash.sh - Shared functions for computing attestation hashes
# =============================================================================
#
# Source this file after defining the PATTERNS array. Then call compute_hash.
#
# REQUIRED BEFORE SOURCING:
#   PATTERNS=( ... )  - Array of glob patterns for files to include in hash
#
# USAGE:
#   PATTERNS=('**/*.hs' 'stack.yaml')
#   source "$(dirname "$0")/lib-attestation-hash.sh"
#   compute_hash           # hash working directory
#   compute_hash <commit>  # hash specific commit
#
# HOW IT WORKS:
#   1. Lists all files matching PATTERNS (sorted for determinism)
#   2. For each file: outputs filename + NUL + content + NUL
#   3. Pipes everything through SHA-256
#
#   Including filenames in the hash ensures renames are detected.
#
# =============================================================================

set -euo pipefail

if [[ -z "${PATTERNS+x}" ]]; then
    echo "Error: PATTERNS array must be defined before sourcing lib-attestation-hash.sh" >&2
    exit 1
fi

if ! command -v rg &>/dev/null; then
    echo "Error: ripgrep (rg) is required but not found in PATH." >&2
    exit 1
fi

cd "$(git rev-parse --show-toplevel)"

# Commit can be passed as argument to compute_hash, or set via ATTESTATION_COMMIT env var
_COMMIT=""

_list_files() {
    if [[ -n "${_COMMIT:-}" ]]; then
        # Use git archive to list files matching glob patterns from a commit
        git archive "$_COMMIT" -- "${PATTERNS[@]}" 2>/dev/null | tar -t 2>/dev/null | grep -v '/$'
    else
        # Use ripgrep to list files matching glob patterns in working directory
        # --hidden needed to find files in .github/
        for pattern in "${PATTERNS[@]}"; do
            rg --files --hidden -g "$pattern" 2>/dev/null || true
        done
    fi | grep -v '\.stack-work' | sort -u
}

_hash_files() {
    # Batch file reads with perl: outputs filename\0content\0filename\0content...
    xargs perl -0777 -ne '
        BEGIN { $first = 1 }
        if (!$first) { print "\0" }
        $first = 0;
        print "$ARGV\0";
        print;
    ' | shasum -a 256 | cut -d' ' -f1
}

compute_hash() {
    _COMMIT="${1:-}"
    if [[ -n "${_COMMIT:-}" ]]; then
        # Extract commit files to temp dir, then hash
        local tmpdir
        tmpdir=$(mktemp -d)
        trap "rm -rf '$tmpdir'" RETURN
        git archive "$_COMMIT" -- "${PATTERNS[@]}" 2>/dev/null | tar -xf - -C "$tmpdir"
        (cd "$tmpdir" && find . -type f | sed 's|^\./||' | grep -v '\.stack-work' | sort -u | _hash_files)
    else
        _list_files | _hash_files
    fi
}
