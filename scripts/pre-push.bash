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
    >&2 echo "The git push operation was canceled because ‘$CMD’ did not complete successfully."
    >&2 echo "To bypass this check, push with ‘--no-verify’."
    exit 1
fi
