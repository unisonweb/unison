#!/usr/bin/env bash
set -euo pipefail
SCRIPTNAME="$(readlink -f -- "${BASH_SOURCE[0]}")"
SCRIPTDIR="$(dirname -- "$SCRIPTNAME")"

function usage {
    echo "Usage: $SCRIPTNAME [-f] [-h]"
    echo
    echo "  -f   Overwrite hooks if they already exist"
    echo "  -h   Show this usage message"
}

trap "echo; usage" ERR

force=("-n")
while getopts ':fh' OPTION; do
    case "$OPTION" in
        f)
            force=("-f")
            ;;
        h)
            usage
            exit 0
            ;;
        \?)
            usage
            exit 1
            ;;
    esac
done

hooks_dir=$(git rev-parse --git-path hooks)

echo "Installing hooks..."
mkdir -p "$hooks_dir"
# this command creates symlink to our pre-commit script
cp "${force[@]}" "$SCRIPTDIR/pre-commit.bash" "$hooks_dir/pre-commit"
cp "${force[@]}" "$SCRIPTDIR/pre-push.bash" "$hooks_dir/pre-push"
echo "Done!"
