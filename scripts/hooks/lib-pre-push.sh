#!/usr/bin/env bash
# =============================================================================
# lib-pre-push.sh - Helper functions for pre-push hook
# =============================================================================
#
# USAGE:
#   source "$REPO_ROOT/scripts/hooks/lib-pre-push.sh"
#
# REQUIRES:
#   REPO_ROOT must be set before sourcing
#
# =============================================================================

# Track files that need to be amended into the commit
declare -a AMEND_FILES=()

# -----------------------------------------------------------------------------
# handle_dirty_worktree <script_path> "$@"
#
# If working directory is dirty, stash changes and re-run the script inside
# with-clean-worktree.sh. Uses PRE_PUSH_CLEAN_WORKTREE env var to prevent
# infinite recursion.
# -----------------------------------------------------------------------------
handle_dirty_worktree() {
    local script_path="$1"
    shift

    if [[ "${PRE_PUSH_CLEAN_WORKTREE:-}" == "1" ]]; then
        return 0
    fi

    if git diff --quiet && git diff --cached --quiet && \
       [[ -z "$(git ls-files --others --exclude-standard)" ]]; then
        return 0
    fi

    echo "Working directory has uncommitted changes."
    echo "Stashing changes to run checks..."
    echo ""

    # Save stdin (push info) to temp file so we can replay it
    local push_info_file
    push_info_file=$(mktemp)
    cat > "$push_info_file"

    # Re-run the script inside the clean worktree wrapper
    exec "$REPO_ROOT/scripts/hooks/with-clean-worktree.sh" \
        bash -c "PRE_PUSH_CLEAN_WORKTREE=1 '$script_path' < '$push_info_file'; EXIT_CODE=\$?; rm -f '$push_info_file'; exit \$EXIT_CODE"
}

# -----------------------------------------------------------------------------
# check_hook_version
#
# Warn if the installed hook differs from the repo version.
# -----------------------------------------------------------------------------
check_hook_version() {
    local source_hook="$REPO_ROOT/scripts/hooks/pre-push.bash"
    if [[ ! -f "$source_hook" ]]; then
        return 0
    fi

    local installed_hash source_hash
    installed_hash=$(shasum "$0" | cut -d' ' -f1)
    source_hash=$(shasum "$source_hook" | cut -d' ' -f1)

    if [[ "$installed_hash" != "$source_hash" ]]; then
        echo "💡 This hook differs from scripts/hooks/pre-push.bash."
        echo "   Run ./scripts/hooks/install.bash -f to sync with the repo version."
        echo ""
    fi
}

# -----------------------------------------------------------------------------
# checkout_branch <local_ref>
#
# Checkout the branch being pushed so that amend updates the correct branch.
# Only acts on refs/heads/* refs, skips if already on the branch.
# -----------------------------------------------------------------------------
checkout_branch() {
    local local_ref="$1"

    if [[ "$local_ref" != refs/heads/* ]]; then
        return 0
    fi

    local branch_name="${local_ref#refs/heads/}"
    local current_branch
    current_branch=$(git symbolic-ref -q --short HEAD 2>/dev/null || true)

    if [[ "$current_branch" != "$branch_name" ]]; then
        echo "Checking out $branch_name..."
        git checkout --quiet "$branch_name"
        echo ""
    fi
}

# -----------------------------------------------------------------------------
# check_proof <name> <local_ref> <local_sha>
#
# Check if proof exists for the given type, run tests if needed.
# Appends to AMEND_FILES array if the proofs file was updated.
#
# Arguments:
#   name      - Proof type (e.g., "transcripts", "tests")
#   local_ref - The ref being pushed (e.g., refs/heads/feature)
#   local_sha - The commit SHA being pushed
# -----------------------------------------------------------------------------
check_proof() {
    local name="$1"
    local local_ref="$2"
    local local_sha="$3"

    local tracked_path=".github/workflows/proofs/${name}.txt"
    local tracked_file="$REPO_ROOT/$tracked_path"

    echo "Checking $name proof for $local_ref..."

    local hash
    hash=$("$REPO_ROOT/scripts/proofs/${name}.sh" --hash "$local_sha")
    echo "  Hash: $hash"

    # Check if proof is already in the committed version
    local committed committed_line
    committed=$(git show "$local_sha:$tracked_path" 2>/dev/null || echo "")
    committed_line=$(echo "$committed" | grep "^$hash " || true)

    if [[ -n "$committed_line" ]]; then
        local committed_result
        committed_result=$(echo "$committed_line" | awk '{print $3}')

        if [[ "$committed_result" == "pass" ]]; then
            echo "  ✅ Already verified (passed)."
            return 0
        elif [[ "$committed_result" == "fail" ]]; then
            cat >&2 <<EOF

❌ $name previously failed for this commit.
   Fix the issues and run ./scripts/proofs/${name}.sh, then push again.
   (You can skip with --no-verify, but CI will fail.)
EOF
            exit 1
        fi
        # Invalid or missing status, fall through to run tests
    fi

    # Run the attestation script
    if ! "$REPO_ROOT/scripts/proofs/${name}.sh"; then
        cat >&2 <<EOF

❌ $name failed. Please fix the issues and try again.
   (You can skip with --no-verify, but CI will likely fail.)
EOF
        exit 1
    fi

    # Only amend when the proof script actually changed the tracked proofs file.
    if ! git diff --quiet -- "$tracked_path"; then
        AMEND_FILES+=("$tracked_file")
    fi
}

# -----------------------------------------------------------------------------
# amend_with_proofs
#
# If AMEND_FILES array has entries, stage them and amend the commit.
# -----------------------------------------------------------------------------
amend_with_proofs() {
    if [[ ${#AMEND_FILES[@]} -eq 0 ]]; then
        return 0
    fi

    echo ""
    echo "📝 Amending commit with updated proofs files..."

    for file in "${AMEND_FILES[@]}"; do
        git add "$file"
    done

    git commit --amend --no-edit --no-verify

    echo "🏁 Done. Please run 'git push' again."
    echo ""
    exit 1
}
