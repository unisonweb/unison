#!/bin/sh

# This script installs the Nix package manager on your system by
# downloading a binary distribution and running its installer script
# (which in turn creates and populates /nix).

{ # Prevent execution if this script was only partially downloaded

unpack=nix-binary-tarball-unpack

require_util() {
    type "$1" > /dev/null 2>&1 || which "$1" > /dev/null 2>&1 ||
        oops "you do not have \`$1' installed, which i need to $2"
}

oops() {
    echo "$0: $@" >&2
    rm -rf "$unpack"
    exit 1
}

case "$(uname -s).$(uname -m)" in
    Linux.x86_64) system=x86_64-linux;;
    Linux.i?86) system=i686-linux;;
    Darwin.x86_64) system=x86_64-darwin;;
    *) oops "sorry, there is no binary distribution of Nix for your platform";;
esac

url="https://nixos.org/releases/nix/nix-1.8/nix-1.8-$system.tar.bz2"

require_util curl "download the binary tarball"
require_util bzcat "decompress the binary tarball"
require_util tar "unpack the binary tarball"

echo "unpacking Nix binary tarball for $system from \`$url'..."
mkdir "$unpack" || oops "failed to create \`$unpack' directory"
curl -L "$url" | bzcat | tar x -C "$unpack" || oops "failed to unpack \`$url'"

[ -e "$unpack"/*/install ] ||
    oops "installation script is missing from the binary tarball!"

"$unpack"/*/install
rm -rf "$unpack"

} # End of wrapping
