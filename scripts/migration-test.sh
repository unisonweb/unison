#!/usr/bin/env bash
# =============================================================================
# migration-test.sh - Check that this ucm can migrate a codebase made by the
# latest release
# =============================================================================
#
# USAGE:
#   ./scripts/migration-test.sh [--ucm PATH] [--keep]
#   ./scripts/migration-test.sh --latest
#
# OPTIONS:
#   --ucm PATH   The development ucm to test (default: $UCM if set, else the
#                binary stack builds from this checkout, built if needed)
#   --keep       Leave the migration-test codebase and the downloaded release
#                in place after a successful run
#   --latest     Print the latest release's tag (e.g. release/1.5.0) and exit
#
# WHAT IT DOES:
#   1. Finds the latest numbered release of unisonweb/unison (pre-releases
#      such as the trunk build are not considered) and downloads its ucm for
#      this platform.
#   2. Runs a transcript with that ucm that installs @unison/base, saving
#      the resulting codebase at ./migration-test.
#   3. Opens the same codebase with the development ucm
#      (`transcript.in-place`, with UNISON_MIGRATION=auto so the migration
#      prompt is skipped) and runs a transcript in it.
#   4. Passes when the development ucm reports "Migrations complete" (or, when
#      the two share a schema version, when nothing needed migrating and the
#      transcript ran); on success the migration-test directory and the
#      download are deleted. Anything else is a failure, with the logs kept.
#
# EXIT CODES:
#   0    the release's codebase migrates and works
#   1    it doesn't
#   125  a prerequisite is missing (no curl or tar, unsupported platform, no
#        network); nothing was tested
#
# Needs curl and tar, and network access to github.com and Unison Share.
# GITHUB_TOKEN, if set, is used for the GitHub API call (CI's rate limit).
#
# =============================================================================

set -euo pipefail

REPO=unisonweb/unison
PREREQ_EXIT=125

cd "$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")/.."

ucm="${UCM:-}"
keep=false
latest_only=false
while [[ $# -gt 0 ]]; do
    case "$1" in
        --ucm) ucm="$2"; shift 2 ;;
        --keep) keep=true; shift ;;
        --latest) latest_only=true; shift ;;
        -h|--help) sed -n '2,41p' "$0"; exit 0 ;;
        *) echo "unknown argument: $1" >&2; exit 2 ;;
    esac
done

prereq() { echo "$*" >&2; exit $PREREQ_EXIT; }

for cmd in curl tar; do
    command -v "$cmd" >/dev/null 2>&1 || prereq "migration-test needs $cmd"
done

# --- the latest release ------------------------------------------------------

api() {
    if [[ -n "${GITHUB_TOKEN:-}" ]]; then
        curl -fsSL -H "Authorization: Bearer $GITHUB_TOKEN" "$@"
    else
        curl -fsSL "$@"
    fi
}

# GitHub's "latest" release is the newest one that is neither a draft nor a
# pre-release, which is what we want: the trunk build is a pre-release.
release_json=$(api "https://api.github.com/repos/$REPO/releases/latest" 2>/dev/null) \
    || prereq "could not reach the GitHub API for the latest release of $REPO"
tag=$(printf '%s' "$release_json" | grep -o '"tag_name": *"[^"]*"' | head -1 | sed 's/.*"\([^"]*\)"$/\1/')
[[ -n "$tag" ]] || prereq "could not read the latest release's tag from the GitHub API"

if [[ "$latest_only" == "true" ]]; then
    echo "$tag"
    exit 0
fi

# --- the platform's asset ----------------------------------------------------

case "$(uname -s)-$(uname -m)" in
    Darwin-arm64)            asset=ucm-macos-arm64.tar.gz ;;
    Darwin-x86_64)           asset=ucm-macos-x64.tar.gz ;;
    Linux-x86_64)            asset=ucm-linux-x64.tar.gz ;;
    Linux-aarch64|Linux-arm64) asset=ucm-linux-arm64.tar.gz ;;
    *) prereq "no release build of ucm for $(uname -s) $(uname -m)" ;;
esac
url=$(printf '%s' "$release_json" | grep -o "\"browser_download_url\": *\"[^\"]*/$asset\"" | head -1 | sed 's/.*"\([^"]*\)"$/\1/')
[[ -n "$url" ]] || prereq "release $tag has no asset named $asset"

# --- the development ucm -----------------------------------------------------

# Without --ucm or UCM, the ucm is the one stack builds from this checkout,
# built here if it isn't yet. Nothing on the PATH is used: a `unison` found
# there is likely to be an installed release, and the test would then compare
# the release with itself.
if [[ -z "$ucm" ]]; then
    command -v stack >/dev/null 2>&1 || prereq "no development ucm: pass --ucm PATH, set UCM, or install stack"
    ucm="$(stack path --local-install-root)/bin/unison"
    if [[ ! -x "$ucm" ]]; then
        echo "Building the development ucm..."
        stack build --fast unison-cli-main:exe:unison || prereq "could not build the development ucm"
    fi
    [[ -x "$ucm" ]] || prereq "stack did not produce $ucm"
fi
# shellcheck disable=SC2086
dev_version=$($ucm --version 2>/dev/null | head -1) || prereq "could not run the development ucm: $ucm"

# --- set up --------------------------------------------------------------------

workdir=$(mktemp -d -t migration-test)
codebase=migration-test
log="$workdir/log"

cleanup_on_success() {
    rm -rf "$codebase" "$workdir"
}

echo "Latest release: $tag ($asset)"
echo "Development ucm: $dev_version"
echo "Work directory: $workdir"

if [[ -e "$codebase" ]]; then
    echo "Removing a previous $codebase directory"
    rm -rf "$codebase"
fi

echo "Downloading $url"
curl -fsSL "$url" -o "$workdir/$asset" || prereq "could not download $url"
mkdir -p "$workdir/release"
tar -xzf "$workdir/$asset" -C "$workdir/release"
release_ucm="$workdir/release/unison/unison"
[[ -x "$release_ucm" ]] || { echo "the release archive has no unison/unison binary" >&2; exit 1; }
echo "Release ucm: $("$release_ucm" --version | head -1)"

# --- 1. a codebase from the release, with base installed ---------------------

cat >"$workdir/install.md" <<'EOF'
``` ucm
scratch/main> lib.install @unison/base
```
EOF

echo ""
echo "Creating $codebase with the release and installing @unison/base..."
if ! "$release_ucm" transcript --save-codebase-to "$codebase" "$workdir/install.md" >"$log.release" 2>&1; then
    echo "The release ucm failed to create the codebase or install base; see $log.release" >&2
    tail -20 "$log.release" >&2
    exit 1
fi

# --- 2. the development ucm on the same codebase -----------------------------

cat >"$workdir/check.md" <<'EOF'
``` ucm
scratch/main> ls lib
```
EOF

echo "Opening $codebase with the development ucm..."
set +e
# shellcheck disable=SC2086
UNISON_MIGRATION=auto $ucm --codebase "$codebase" transcript.in-place "$workdir/check.md" >"$log.dev" 2>&1
dev_exit=$?
set -e

if grep -q "Migrations complete" "$log.dev"; then
    migrated=yes
elif [[ $dev_exit -eq 0 ]] && ! grep -q "Migrating codebase" "$log.dev"; then
    migrated=none
else
    migrated=no
fi

case "$migrated" in
    yes)
        if [[ $dev_exit -ne 0 ]]; then
            echo "The codebase was migrated but the transcript failed afterwards (exit $dev_exit); see $log.dev" >&2
            tail -20 "$log.dev" >&2
            exit 1
        fi
        grep "Migrating codebase" "$log.dev" || true
        echo "Migration complete: the development ucm migrated the release's codebase and ran a transcript in it."
        ;;
    none)
        echo "Nothing to migrate: the development ucm opened the release's codebase without a migration and ran a transcript in it."
        ;;
    no)
        echo "The development ucm did not complete the migration (exit $dev_exit); see $log.dev" >&2
        tail -30 "$log.dev" >&2
        exit 1
        ;;
esac

if [[ "$keep" == "true" ]]; then
    echo "Kept $codebase and $workdir"
else
    cleanup_on_success
fi
