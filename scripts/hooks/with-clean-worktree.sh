#!/bin/bash
# =============================================================================
# with-clean-worktree.sh - Run a command on a clean worktree, preserving state
# =============================================================================
#
# USAGE:
#   ./scripts/with-clean-worktree.sh <command> [args...]
#
# PURPOSE:
#   Temporarily stashes all local changes (staged, unstaged, and untracked),
#   runs the specified command on a clean working tree, then restores
#   everything exactly as it was—including the distinction between staged
#   and unstaged changes.
#
#   The command is free to checkout different branches/commits as needed.
#   After the command completes, we restore to the original ref.
#
# EXAMPLES:
#   ./scripts/with-clean-worktree.sh ./scripts/proofs/transcripts.sh
#   ./scripts/with-clean-worktree.sh bash -c 'git checkout feature && make test'
#
# HOW IT WORKS:
#   1. Saves the current branch/ref and HEAD
#   2. Saves staged changes as a patch (HEAD -> index)
#   3. Saves unstaged changes as a patch (index -> working tree)
#   4. Saves untracked files to a tarball
#   5. Resets working tree to clean state
#   6. Runs the provided command (may checkout other refs)
#   7. Checks out back to original ref
#   8. Restores untracked files, staged patch, and unstaged patch
#
# =============================================================================

set -euo pipefail

if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <command> [args...]" >&2
    exit 1
fi

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Create temp directory for saved state
TEMP_DIR=$(mktemp -d)
cleanup() {
    rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

# -----------------------------------------------------------------------------
# Save original ref (branch name or commit sha)
# -----------------------------------------------------------------------------
ORIGINAL_REF=$(git symbolic-ref -q HEAD 2>/dev/null || git rev-parse HEAD)
ORIGINAL_HEAD=$(git rev-parse HEAD)

# -----------------------------------------------------------------------------
# Check if there's anything to stash
# -----------------------------------------------------------------------------
has_staged_changes() {
    ! git diff --cached --quiet
}

has_unstaged_changes() {
    ! git diff --quiet
}

has_untracked_files() {
    [[ -n "$(git ls-files --others --exclude-standard)" ]]
}

NEED_RESTORE=false

if has_staged_changes || has_unstaged_changes || has_untracked_files; then
    NEED_RESTORE=true
    echo "Saving working directory state..."

    # 1. Save staged changes as patch (HEAD -> index)
    if has_staged_changes; then
        git diff --cached > "$TEMP_DIR/staged.patch"
        echo "  Saved staged changes"
    fi

    # 2. Save unstaged changes as patch (index -> working tree)
    if has_unstaged_changes; then
        git diff > "$TEMP_DIR/unstaged.patch"
        echo "  Saved unstaged changes"
    fi

    # 3. Save untracked files
    if has_untracked_files; then
        git ls-files --others --exclude-standard -z > "$TEMP_DIR/untracked.list"
        # Use tar with null-delimited input
        tar -cf "$TEMP_DIR/untracked.tar" --null -T "$TEMP_DIR/untracked.list" 2>/dev/null || true
        echo "  Saved untracked files"
    fi

    # 4. Reset to clean state
    echo "Cleaning working directory..."
    git reset --hard HEAD
    git clean -fd
    echo ""
fi

# -----------------------------------------------------------------------------
# Run the command
# -----------------------------------------------------------------------------
CMD_EXIT=0
"$@" || CMD_EXIT=$?

# -----------------------------------------------------------------------------
# Restore to original state
# -----------------------------------------------------------------------------

# 5. Checkout back to original ref (command may have switched branches)
CURRENT_HEAD=$(git rev-parse HEAD)
if [[ "$CURRENT_HEAD" != "$ORIGINAL_HEAD" ]] || \
   [[ "$(git symbolic-ref -q HEAD 2>/dev/null || true)" != "$ORIGINAL_REF" ]]; then
    echo ""
    echo "Returning to original ref..."
    if [[ "$ORIGINAL_REF" == refs/heads/* ]]; then
        # Was on a branch
        git checkout --quiet "${ORIGINAL_REF#refs/heads/}"
    else
        # Was detached
        git checkout --quiet "$ORIGINAL_HEAD"
    fi
fi

if [[ "$NEED_RESTORE" == "true" ]]; then
    echo ""
    echo "Restoring working directory state..."

    # 6. Restore untracked files first (before patches might fail)
    if [[ -f "$TEMP_DIR/untracked.tar" ]]; then
        tar -xf "$TEMP_DIR/untracked.tar" 2>/dev/null || true
        echo "  Restored untracked files"
    fi

    # 7. Restore staged changes to index and working tree
    if [[ -f "$TEMP_DIR/staged.patch" && -s "$TEMP_DIR/staged.patch" ]]; then
        if git apply --cached "$TEMP_DIR/staged.patch" 2>/dev/null; then
            echo "  Restored staged changes"
            # Sync working tree to index so unstaged patch can apply correctly
            git checkout-index -a -f
        else
            echo "  Warning: Could not restore staged changes cleanly" >&2
            echo "  Patch saved to: $TEMP_DIR/staged.patch" >&2
            # Don't delete temp dir on failure
            trap - EXIT
        fi
    fi

    # 8. Restore unstaged changes to working tree
    if [[ -f "$TEMP_DIR/unstaged.patch" && -s "$TEMP_DIR/unstaged.patch" ]]; then
        if git apply "$TEMP_DIR/unstaged.patch" 2>/dev/null; then
            echo "  Restored unstaged changes"
        else
            echo "  Warning: Could not restore unstaged changes cleanly" >&2
            echo "  Patch saved to: $TEMP_DIR/unstaged.patch" >&2
            # Don't delete temp dir on failure
            trap - EXIT
        fi
    fi
fi

exit $CMD_EXIT
