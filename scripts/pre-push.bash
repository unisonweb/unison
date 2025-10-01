#!/bin/bash
set -euo pipefail

# Run `./install-hooks.bash` to install this pre-push hook.

# Check if we actually have commits to push
commits=$(git log "@{u}..")
if [ -z "$commits" ]; then
    exit 0
fi

CMD="./scripts/test.sh"
if ! "$CMD"; then
    echo "The git push operation was canceled because ‘$CMD’ did not complete successfully."
    exit 1
fi
