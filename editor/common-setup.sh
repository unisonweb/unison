# This file should not be run directly; it exists to share setup code between the various scripts in try-reflex
# Before running this script, DIR should be defined equal to the directory containing this script

REPO="https://github.com/unisonweb/platform"

NIXOPTS="--option extra-binary-caches https://ryantrinkle.com:5443/ -j 8"

trap "echo 'It looks like a problem occurred.  Please submit an issue at $REPO/issues'; exit 1" ERR

echo "If you have any trouble with this script, please submit an issue at $REPO/issues"

(

cd "$DIR"

if [ ! -d /nix ] ; then
  if ! type -P curl >/dev/null ; then
    echo "Please make sure that 'curl' is installed and can be run from this shell"
    exit 1
  fi

  echo "In order to continue, $0 must install the Nix package manager.  This requires root access, so you will be prompted for your password.  If you do not wish to continue, just hit Ctrl-C at the password prompt."
  ./installNix.sh
fi

)

if ! type -P nix-shell >/dev/null ; then
  . ~/.nix-profile/etc/profile.d/nix.sh
  if ! type -P nix-shell >/dev/null ; then
    echo "It looks like Nix isn't working.  Please make sure you can run nix-shell, then retry the $0, or submit an issue at $REPO/issues"
    exit 1
  fi
fi

(

cd "$DIR"


if ! type -P git >/dev/null ; then
  echo "Please make sure that 'git' is installed and can be run from this shell"
  exit 1
fi

for x in nixpkgs reflex reflex-dom ; do
  if [ ! "$(ls -A "$x")" ] ; then

    git submodule update --init --recursive "$x"
  fi
done

)
