#!/bin/bash

set -e

if ! command -V "gh" >/dev/null 2>&1; then
   echo "Required command \`gh\` not found, find installation instructions here: https://cli.github.com/manual/installation"
   exit 1
fi

if ! [[ "$1" =~ ^M[0-9]+[a-z]?$ ]] ; then
 echo "Version tag be of the form 'M4' or 'M4a'"
 echo "E.g. "
 echo "$0 M4a"
 exit 1
fi

version="${1}"
committish=${2:trunk}

echo "Tagging current unison-local-ui revision for this release..."
gh release create "release/${version}" --repo unisonweb/unison-local-ui --target main --generate-notes

echo "Tagging ${committish} with ${version} in the unison repo"
git tag "${version}" "${committish}"
git push origin "${version}"


echo "Kicking off Homebrew update task"
gh workflow run release_version --field "version=${version}"
