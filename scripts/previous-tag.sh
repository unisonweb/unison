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

if ! [[ "$1" =~ ^M[0-9]+[a-z]?$ ]] ; then
 echo "Version tag must be of the form 'M4' or 'M4a'. E.g."
 echo "$0 M4a"
 exit 1
fi

echo "$1" | $awk_exe '
        # This script figures out a previous tag for a given release to make release notes from.

        # The previous release of something like M4a is just M4
        match($0, /^(\w[0-9]+)a$/, a) {print a[1]}
        # The previous release of something like M4 is M3, since we want to show off everything since the last major release
        match($0, /^\w([0-9]+)$/, a) {print "M" a[1] - 1}
        # The previous release of something like M4b is M4a
        match($0, /^(\w[0-9]+)([b-z])$/, a) {printf a[1]; system("echo " a[2] " | tr b-z a-y")}
'
