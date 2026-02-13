#!/usr/bin/env bash
set -euo pipefail
SCRIPTDIR="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

force=false
while getopts ':fh' OPTION; do
    case "$OPTION" in
        f)
            force=true
            ;;
        h|?)
            echo "Usage: $0 [-f] [-h]"
            echo
            echo "  -f   Overwrite hooks if they already exist"
            echo "  -h   Show this usage message"
            exit 0
            ;;
    esac
done

hooks_dir=$(git rev-parse --git-path hooks)
mkdir -p "$hooks_dir"

# Check each hook
hooks=("pre-commit:pre-commit.bash" "pre-push:pre-push.bash")
to_install=()
different=()

for entry in "${hooks[@]}"; do
    hook_name="${entry%%:*}"
    source_file="${entry##*:}"
    installed="$hooks_dir/$hook_name"
    source="$SCRIPTDIR/$source_file"

    if [[ ! -f "$installed" ]]; then
        to_install+=("$hook_name")
    elif ! diff -q "$installed" "$source" > /dev/null 2>&1; then
        different+=("$hook_name")
    fi
done

# If hooks exist but differ, and no -f flag
if [[ ${#different[@]} -gt 0 ]] && [[ "$force" != true ]]; then
    echo "You have existing hooks that differ from the ones in this repo:"
    echo ""
    for hook_name in "${different[@]}"; do
        echo "  $hook_name:"
        diff "$hooks_dir/$hook_name" "$SCRIPTDIR/$hook_name.bash" 2>/dev/null | head -10 | sed 's/^/    /' || true
        echo ""
    done
    echo "To replace them with the repo versions, run again with -f."
    exit 1
fi

# Nothing to do
if [[ ${#to_install[@]} -eq 0 ]] && [[ ${#different[@]} -eq 0 ]]; then
    echo "The hooks are already installed and up to date."
    echo ""
    echo "They'll run automatically on commit and push."
    exit 0
fi

# Install hooks
for entry in "${hooks[@]}"; do
    hook_name="${entry%%:*}"
    source_file="${entry##*:}"
    cp -f "$SCRIPTDIR/$source_file" "$hooks_dir/$hook_name"
done

echo "Done! The hooks are installed and will run automatically on commit and push."
