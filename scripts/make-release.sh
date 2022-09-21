#!/bin/bash

set -e

usage() {
    echo "Usage: $0 <version>"
    echo ""
    echo "E.g."
    echo "$0 M4a"
}

if [[ -z "$1"  ]] ; then
  usage
  exit 1
fi

if ! command -V "gh" >/dev/null 2>&1; then
   echo "Required command \`gh\` not found, find installation instructions here: https://cli.github.com/manual/installation"
   exit 1
fi

if ! [[ "$1" =~ ^M[0-9]+[a-z]?$ ]] ; then
 echo "Version tag must be of the form 'M4' or 'M4a'"
 usage
 exit 1
fi

version="${1}"
committish=${2:-trunk}

prereleaseFlag=""

if [[ "$1" =~ ^M[0-9]+[a-z]$ ]] ; then
    prereleaseFlag="--prerelease"
fi

gh workflow run release --repo unisonweb/unison --field "version=${version}" --ref cp/automate-minor-releases

echo "Tagging current unison-local-ui revision for this release..."
gh release create "release/${version}" --repo unisonweb/unison-local-ui --target main --generate-notes $prereleaseFlag

echo "Tagging ${committish} with ${version} in the unison repo"
git tag "${version}" "${committish}"
git push origin "${version}"

echo "Kicking off Homebrew update task"
gh workflow run release_version --repo unisonweb/homebrew-unison --field "version=${version}" --ref cp/automate-brew-upgrade

echo "Opening relevant workflows in browser"
gh workflow view release_version --web --repo unisonweb/homebrew-unison || true
gh workflow view release --web --repo unisonweb/unison || true

echo "Okay! All the work has been kicked off, it may take several hours to complete."
