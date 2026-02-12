#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"

# Check if this hook differs from the repo version
SOURCE_HOOK="$REPO_ROOT/scripts/hooks/pre-commit.bash"
if [[ -f "$SOURCE_HOOK" ]]; then
    INSTALLED_HASH=$(shasum "$0" | cut -d' ' -f1)
    SOURCE_HASH=$(shasum "$SOURCE_HOOK" | cut -d' ' -f1)
    if [[ "$INSTALLED_HASH" != "$SOURCE_HASH" ]]; then
        echo "💡 This hook differs from scripts/hooks/pre-commit.bash."
        echo "   Run ./scripts/hooks/install.bash -f to sync with the repo version."
        echo ""
    fi
fi

if [[ "${SKIP_PRECOMMIT:-}" == "1" ]]; then
    echo "Skipping pre-commit checks (SKIP_PRECOMMIT=1)"
    exit 0
fi

echo "Running pre-commit checks..."

# Run each check, recording pass/fail (don't block commit on failure)
for proof_type in tests transcripts; do  # formatting, weeds, left off for now
    ./scripts/proofs/${proof_type}.sh || true
done

# Stage the proof files so they're included in this commit
git add .github/workflows/proofs/*.txt 2>/dev/null || true
