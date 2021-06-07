echo "This script downloads the latest codebase UI release"
echo "and puts it in the correct spot next to the unison"
echo "executable built by stack."
echo ""

stack build
curl -L https://github.com/unisonweb/codebase-ui/releases/download/latest/ucm.zip --output ucm.zip
parent_dir="$(dirname -- $(stack exec which unison))"
mkdir -p "$parent_dir/ui"
unzip -o ucm.zip -d "$parent_dir/ui"
