#!/bin/bash

# E.g.
# ./previous-tag.sh M4 -> M3
# ./previous-tag.sh M4a -> M4
# ./previous-tag.sh M4b -> M4a

awk_exe="awk"

# if gawk exists, use that:
if command -V "gawk" >/dev/null 2>&1; then
   awk_exe=gawk
fi

if ! ("$awk_exe" --version | grep GNU) >/dev/null 2>&1; then
   echo "GNU awk is required, install with \`brew install gawk\`"
   exit 1
fi

input_version="$1"

if ! [[ "$input_version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] ; then
 echo "Version tag must be of the form 'x.y.z' where x, y, and z are nonnegative integers. e.g."
 echo "$0 0.5.11"
 exit 1
fi

if [[ "$input_version" == "0.5.11" ]]; then
   echo "M5j"
else
   IFS='.' read -r -a version_parts <<< "$input_version"
   major=${version_parts[0]}
   minor=${version_parts[1]}
   patch=${version_parts[2]}

   if [[ "$patch" -gt 0 ]]; then
      patch=$((patch - 1))
      echo "$major.$minor.$patch"
   elif [[ "$minor" -gt 0 ]]; then
      minor=$((minor - 1))
      tag=$(git tag --list "release/$major.$minor.*" | sort -r | head -n 1)
      echo "${tag#release/}"
   elif [[ "$major" -gt 0 ]]; then
      major=$((major - 1))
      tag=$(git tag --list "release/$major.*" | sort -r | head -n 1)
      echo "${tag#release/}"
   else
      echo "Idk what to do with $input_version".
   fi
fi
