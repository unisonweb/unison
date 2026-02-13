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

# If dirty, stash and re-run in clean worktree
handle_dirty_worktree "$0" "$@"

# Warn if hook is out of date
check_hook_version

# Process each ref being pushed
while read -r local_ref local_sha remote_ref remote_sha; do
    # Skip branch deletions
    [[ "$local_sha" == "0000000000000000000000000000000000000000" ]] && continue

    # Checkout branch so amend updates the correct ref
    checkout_branch "$local_ref"

    # Check each proof type
    for proof_type in transcripts tests; do # formatting, weeds, left of for now
        check_proof "$proof_type" "$local_ref" "$local_sha"
    done
done

# Amend commit if any proofs were updated
amend_with_proofs
