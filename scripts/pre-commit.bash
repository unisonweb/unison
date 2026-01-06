#!/usr/bin/env bash
set -euo pipefail

echo "Running pre-commit hook from $(pwd)"

if ! ./scripts/test.sh; then
    >&2 echo "Tests must pass before commit!"
    >&2 echo "To bypass this check, commit with ‘--no-verify’."
    exit 1
fi
