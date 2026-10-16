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
#   With HASH_NORMALIZE=true set before sourcing, Haskell and C sources are
#   hashed with their comments removed (pragmas kept) and all whitespace
#   collapsed, so that a comment edit or a reformatting does not invalidate an
#   attestation. The proofs that only depend on what the code does opt in;
#   the formatting proof hashes the raw text.
#
# =============================================================================

set -euo pipefail

if [[ -z "${PATTERNS+x}" ]]; then
    echo "Error: PATTERNS array must be defined before sourcing lib-attestation-hash.sh" >&2
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
        # List tracked + untracked files matching glob patterns (respects .gitignore)
        git ls-files --cached --others --exclude-standard -- "${PATTERNS[@]}"
    fi | LC_ALL=C sort -u
}

# NUL-delimited version for safe piping to xargs -0
_list_files_0() {
    _list_files | tr '\n' '\0'
}

_hash_files() {
    # Batch file reads with perl: outputs filename\0content\0filename\0content...
    # Input is NUL-delimited file paths (from _list_files_0)
    HASH_NORMALIZE="${HASH_NORMALIZE:-false}" xargs -0 perl -0777 -ne '
        BEGIN { $first = 1 }
        if (!$first) { print "\0" }
        $first = 0;
        print "$ARGV\0";
        if ($ENV{HASH_NORMALIZE} eq "true") {
            if ($ARGV =~ /\.hs$/) {
                # drop line and block comments, keep strings and pragmas
                s{("(?:[^"\\\n]|\\.)*")|(\{-#.*?#-\})|\{-.*?-\}|(?<![!#\$%&*+.\/<=>?@\\^|~:-])--+(?![!#\$%&*+.\/<=>?@\\^|~:])[^\n]*}{defined $1 ? $1 : defined $2 ? $2 : ""}gse;
                s/\s+/ /g;
            } elsif ($ARGV =~ /\.[ch]$/) {
                s{("(?:[^"\\\n]|\\.)*")|\/\*.*?\*\/|\/\/[^\n]*}{defined $1 ? $1 : ""}gse;
                s/\s+/ /g;
            }
        }
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
        (cd "$tmpdir" && find . -type f | sed 's|^\./||' | grep -v '\.stack-work' | LC_ALL=C sort -u | tr '\n' '\0' | _hash_files)
    else
        _list_files_0 | _hash_files
    fi
}

# Check if attestation exists in the tracked proofs file.
# Prints: <status> <hash>
# Statuses: pass, fail, partial (only some transcripts were run, see <name>-partial.txt),
# nofile (proofs file absent), noentry (file exists but hash not found)
_check_attestation() {
    local name="$1"
    local proofs_file=".github/workflows/proofs/${name}.txt"
    local hash
    hash=$(compute_hash)

    if [[ ! -f "$proofs_file" ]]; then
        echo "nofile $hash"
        return 0
    fi

    local result
    result=$(grep "^$hash " "$proofs_file" 2>/dev/null | awk '{print $3}' || true)
    if [[ "$result" == "pass" || "$result" == "fail" || "$result" == "partial" ]]; then
        echo "$result $hash"
    else
        echo "noentry $hash"
    fi
}

# Handle --hash and --check flags early, before the full attestation machinery.
# Derives the attestation name from the calling script's filename.
# Usage (in each proof script, after defining PATTERNS):
#   source "$(dirname "$0")/lib-attestation-hash.sh"
#   handle_flags "$@"
handle_flags() {
    local name
    name=$(basename "${BASH_SOURCE[1]}" .sh)
    case "${1:-}" in
        --hash)       compute_hash "${2:-}"; exit 0 ;;
        --check)      _check_attestation "$name"; exit 0 ;;
        --list-files) _COMMIT="${2:-}"; _list_files; exit 0 ;;
    esac
}
