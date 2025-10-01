#!/usr/bin/env bash
set -euo pipefail

echo "Running pre-commit hook from $(pwd)"

if ! ./scripts/test.sh; then
    echo "Tests must pass before commit!"
    exit 1
fi
