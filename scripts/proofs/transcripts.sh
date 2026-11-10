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
#   --only PREFIX   Run only the transcripts whose file name starts with
#                   PREFIX (may be repeated; the special names round-trip,
#                   docs.to-html and fix5507 select the manual ones). The
#                   attestation is then recorded as "partial", with the
#                   transcripts run and their hashes listed next to it in
#                   .github/workflows/proofs/transcripts/<hash>-partial.txt. A partial
#                   attestation satisfies the pull request check but not the
#                   merge queue, which runs everything, and never makes a
#                   later full run skip. A prefix that selects jit-tests.md
#                   also runs it with the JIT on, as the full run does.
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
    # Native sources and headers (affect runtime behavior)
    '**/*.c'
    '**/*.h'
    # The JIT's embedded IR (jit-tests.md is run with the JIT on)
    '**/*.ll'
    # Transcript inputs (need both *.ext and **/*.ext because ** doesn't match zero dirs)
    'unison-src/*.md'
    'unison-src/**/*.md'
    'unison-src/*.u'
    'unison-src/**/*.u'
    # This script (for hash integrity)
    '**/transcripts.sh'
)
# Note: the partial list is deliberately not hashed: it describes the run, not the inputs.

# Hash sources without their comments and whitespace (see lib-attestation-hash.sh)
HASH_NORMALIZE=true

# Handle --hash and --check early, before sourcing other libraries
source "$(dirname "$0")/lib-attestation-hash.sh"
handle_flags "$@"

# --only PREFIX ... selects a subset; the attestation is then partial
ONLY=()
ARGS=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --only) ONLY+=("$2"); shift 2 ;;
        *) ARGS+=("$1"); shift ;;
    esac
done
set -- "${ARGS[@]+"${ARGS[@]}"}"

source "$(dirname "$0")/lib-attestation-randr.sh"
attestation_setup "transcripts" "$@"
attestation_check_skip
attestation_require_commands stack

PARTIAL_FILE="$TRACKED_PROOFS_DIR/$HASH-partial.txt"

# The transcript files a prefix selects, across every directory the runner
# scans, for the partial record.
selected_transcripts() {
    local prefix="$1"
    case "$prefix" in
        round-trip) echo unison-src/transcripts-round-trip/main.md; echo unison-src/transcripts-manual/rewrites.md ;;
        docs.to-html) echo unison-src/transcripts-manual/docs.to-html.md ;;
        fix5507) echo unison-src/tests/fix5507.md ;;
        *)
            for dir in unison-src/transcripts unison-src/transcripts/idempotent unison-src/transcripts/errors unison-src/transcripts-using-base; do
                for f in "$dir/$prefix"*.md; do
                    [[ -f "$f" && "$f" != *.output.md ]] && echo "$f"
                done
            done
            ;;
    esac
}

# === Tool-specific commands ===
# Run in subshell to capture exit status while allowing errexit inside
set +e
(
    set -euo pipefail

    echo "Building ucm and transcripts binaries..."
    stack build --fast unison-cli:exe:transcripts unison-cli-main:exe:unison --test --no-run-tests

    UCM=$(stack exec -- which unison)
    TRANSCRIPTS=$(stack exec -- which transcripts)

    run_round_trip() {
        echo ""
        echo "Running round-trip tests..."
        "$UCM" transcript unison-src/transcripts-round-trip/main.md
        "$UCM" transcript unison-src/transcripts-manual/rewrites.md
        echo "Checking for round-trip diffs..."
        git diff --ignore-cr-at-eol --exit-code \
            unison-src/transcripts-round-trip/main.output.md \
            unison-src/transcripts-manual/rewrites.output.md
    }
    run_main() {
        echo ""
        echo "Running main transcripts${1:+ matching $1}..."
        "$TRANSCRIPTS" ${1:+"$1"}
        echo "Checking for transcript diffs..."
        git diff --ignore-cr-at-eol --exit-code unison-src/transcripts
    }
    run_docs() {
        echo ""
        echo "Running docs.to-html transcript..."
        "$UCM" transcript unison-src/transcripts-manual/docs.to-html.md
        echo "Checking for docs diffs..."
        git diff --ignore-cr-at-eol --exit-code \
            unison-src/transcripts-manual/docs.to-html.output.md \
            unison-src/transcripts-manual/docs.to-html
    }
    # jit-tests.md is run by the transcript runner with the JIT off like any
    # other; this runs it again with the JIT on (and a low threshold, so that
    # the short loops compile too), so that a change that breaks native code
    # shows up here and not in full CI. A machine without LLVM gets a warning,
    # not a failure: the run then only repeats the interpreter's, and the CI
    # jit job is the one that insists on native code.
    run_jit() {
        echo ""
        echo "Running jit-tests.md with the JIT on..."
        local dir
        dir=$(mktemp -d)
        cp unison-src/transcripts/idempotent/jit-tests.md "$dir/jit-tests.md"
        # the transcript uses only builtins, so a fresh codebase does
        UNISON_JIT=on UNISON_JIT_THRESHOLD=5 UNISON_JIT_LOG=1 "$UCM" -C "$dir/codebase" transcript.fork "$dir/jit-tests.md" 2>&1 | tee "$dir/jit.log"
        if ! grep -q 'JIT compilation activated' "$dir/jit.log" || grep -q 'the JIT is off' "$dir/jit.log"; then
            echo "warning: the JIT did not activate (see the log above), so this only repeated the interpreter's run" >&2
        elif ! grep -q ': compiling ' "$dir/jit.log"; then
            echo "warning: the JIT was on but compiled nothing (see the log above)" >&2
        fi
        echo "Checking for JIT transcript diffs..."
        diff --strip-trailing-cr unison-src/transcripts/idempotent/jit-tests.md "$dir/jit-tests.output.md"
        rm -rf "$dir"
    }
    run_fix5507() {
        echo ""
        echo "Running fix5507 regression test..."
        "$UCM" transcript unison-src/tests/fix5507.md
        "$UCM" run.compiled fix5507.uc
    }

    if [[ ${#ONLY[@]} -eq 0 ]]; then
        run_round_trip
        run_main
        run_jit
        run_docs
        run_fix5507
    else
        for prefix in "${ONLY[@]}"; do
            case "$prefix" in
                round-trip) run_round_trip ;;
                docs.to-html) run_docs ;;
                fix5507) run_fix5507 ;;
                *)
                    if [[ -z "$(selected_transcripts "$prefix")" ]]; then
                        echo "No transcript starts with '$prefix'" >&2
                        exit 1
                    fi
                    run_main "$prefix"
                    # (not a pipe into grep -q: with pipefail, grep closing the
                    # pipe early would fail the condition)
                    if [[ "$(selected_transcripts "$prefix")" == *"/jit-tests.md"* ]]; then
                        run_jit
                    fi
                    ;;
            esac
        done
    fi
)
status=$?

if [[ ${#ONLY[@]} -eq 0 ]]; then
    # a full run supersedes any partial record (recording clears the directory)
    attestation_record_and_exit $status
elif [[ $status -eq 0 ]]; then
    # record a partial attestation, then which transcripts were run, with their
    # hashes, next to it (recording first, since it clears the directory)
    attestation_record partial
    {
        echo "# transcripts run for the partial attestation $HASH"
        for prefix in "${ONLY[@]}"; do
            selected_transcripts "$prefix"
        done | LC_ALL=C sort -u | while read -r f; do
            echo "$(shasum -a 256 "$f" | cut -d' ' -f1) $f"
        done
    } > "$PARTIAL_FILE"
    echo ""
    echo "Partial run: $(grep -c -v '^#' "$PARTIAL_FILE") transcript(s) recorded in $PARTIAL_FILE"
    echo "Commit $PARTIAL_FILE with the proofs directory."
else
    attestation_record_and_exit $status
fi
