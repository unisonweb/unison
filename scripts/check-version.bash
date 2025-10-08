SCRIPTNAME="$(readlink -f -- "${BASH_SOURCE[0]}")"
SCRIPTDIR="$(dirname -- "$SCRIPTNAME")"

# shellcheck disable=SC2120
# because the argument is optional
function indent() {
    local indentSize=2
    local indent=${1:-1}
    pr -to $((indent * indentSize))
}

## This function checks
## 1. that the command is on PATH,
## 2. that the version of the command found matches our expectation (command must support `--version`), and
## 3. if the user has Nix, that our expectation matches the version specified by our Nix config.
##
## Failing either of the first two checks causes an error, while the third is only a warning (because the third
## indicates a code issue, not a user issue).
function version-equals {
    local command="$1"
    ## This would ideally be read from ./nix/versions.nix, but we can’t assume the user has Nix, so we hardcode this,
    ## then at the end of the script, if the user _does_ have Nix, we check if this hardcoded version is the one we
    ## want.
    local expected_version="$2"

    if command -v "$command" >/dev/null; then
        if ! "$command" --version | grep -q "${expected_version}"; then
            >&2 echo "Skipping $command check – found, but the version isn’t ${expected_version}:"
            "$command" --version | indent >&2
            exit 78
        fi
    else
        >&2 echo "Skipping $command check – not found on PATH"
        exit 127
    fi

    ## When the user has Nix, check that `expected_version` matches what we require for Nix.
    if command -v nix >/dev/null; then
        nix_reported_version="$(nix eval --file $SCRIPTDIR/../nix/versions.nix "$command" --raw)"
        if [[ "$expected_version" != "$nix_reported_version" ]]; then
            >&2 echo "⚠️ Running $command check with unexpected version."
            >&2 echo "  Please update ‘version-equals ${command} \"$expected_version\"’ called from"
            >&2 echo "  $SCRIPTNAME"
            >&2 echo "  to use \"$nix_reported_version\" and open a PR."
        fi
    fi
}
