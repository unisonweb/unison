#!/bin/bash
# =============================================================================
# pre-push.bash - Git pre-push hook for proof verification
# =============================================================================
#
# Run `./scripts/hooks/install.bash` to install this pre-push hook.
#
# PURPOSE:
#   Ensures proofs (transcripts, tests, etc.) have been verified before pushing.
#   If proofs need to be generated, runs them and amends the commit.
#
# EXAMPLES:
#   git push                    # Normal push with all checks
#   git push --no-verify        # Skip all pre-push checks (use with caution)
#
# =============================================================================

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
source "$REPO_ROOT/scripts/hooks/lib-pre-push.sh"

# Warn if hook is out of date
check_hook_version

# Process each ref being pushed
while read -r local_ref local_sha remote_ref remote_sha; do
    # Skip branch deletions
    [[ "$local_sha" == "0000000000000000000000000000000000000000" ]] && continue

    # Pass 1: check hashes (cheap, no clean worktree needed)
    declare -a needs_run=()
    for proof_type in transcripts tests; do
        if ! check_proof_hash "$proof_type" "$local_ref" "$local_sha"; then
            needs_run+=("$proof_type")
        fi
    done

    # Pass 2: if any proofs need running, stash dirty worktree and run them
    if [[ ${#needs_run[@]} -gt 0 ]]; then
        handle_dirty_worktree "$0" "$@"
        checkout_branch "$local_ref"
        for proof_type in "${needs_run[@]}"; do
            run_proof "$proof_type" "$local_ref" "$local_sha"
        done
    fi
done

# Amend commit if any proofs were updated
amend_with_proofs
