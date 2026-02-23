## Building with Nix

__NB__: It’ll speed things up if the Unison Nix cache is trusted when building, otherwise you’ll likely end up building hundreds of packages Haskell packages.

The recommended way to do this is to add the public key and URL for the cache to your system’s Nix configuration. /etc/nix/nix.conf should have lines similar to
```conf
trusted-public-keys = unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k=
trusted-substituters = https://unison.cachix.org
```
these lines could be prefixed with `extra-` and they may have additional entries besides the ones for our cache.

This command should work if you don’t want to edit the file manually:
```shell
sudo sh -c 'echo "extra-trusted-public-keys = unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k=
extra-trusted-substituters = https://unison.cachix.org" >>/etc/nix/nix.conf'
```
After updating /etc/nix/nix.conf, you need to restart the Nix daemon. To do this on
- Ubuntu: `sudo systemctl restart nix-daemon`
- MacOS:
    ```shell
    sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    ```

If you use NixOS, you may instead add this via your configuration.nix with
```nix
nix.settings.trusted-public-keys = ["unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k="];
nix.settings.trusted-substituters = ["https://unison.cachix.org"];
```
and run `sudo nixos-rebuild switch` afterward.

It’s _not_ recommended to add your user to `trusted-users`. This _can_ make enabling flake configurations simpler (like the Unison Nix cache here), but [it’s equivalent to giving that user root access (without need for sudo)](https://nix.dev/manual/nix/2.23/command-ref/conf-file.html#conf-trusted-users).

## Building package components with nix

### Build the unison executable

```shell
nix build
```

### Build a specific package

```shell
nix build '.#unison-cli-integration'
nix build '.#unison-syntax'
nix build '.#unison-cli'
```

### Development environments

#### Get into a development environment for building with Stack or Cabal
This gets you into a development environment with the preferred
versions of the compiler and other development tools. These
include:

- Cabal
- GHC
- haskell-language-server
- hpack
- Ormolu
- Stack
- Weeder

```shell
nix develop
```

#### Get into a development environment for building a specific package
This gets you into a development environment with the preferred
versions of the compiler and other development tools. Additionally,
all haskell dependencies of this package are provided by the nix shell
(including profiling dependencies).

```shell
nix develop '.#<package-name>'
```

for example:

```shell
nix develop '.#unison-cli'
```
or
```shell
nix develop '.#unison-parser-typechecker'
```

This is useful if you wanted to profile a package. For example, if you
want to profile `unison-cli-main:exe:unison` then you could get into one of these
shells, cd into its directory, then run the program with
profiling.

```shell
nix develop '.#unison-parser-typechecker'
cd unison-cli
cabal run --enable-profiling unison-cli-main:exe:unison -- +RTS -p
```

## updating the Nix cache

There is [a cache for Unison artifacts on Cachix](https://unison.cachix.org). It is primarily used for two purposes:

1. to keep an up-to-date development environment for Unison contributors and
2. to maintain built versions of the last few releases.

Correspondingly, CI automatically updates the cache on merges to trunk (to satisfy the first use case) and on release tags (to mostly satisfy the second).

### updating when the development environment changes

Any PR that affects the Nix development environment (e.g., touches flake.nix or files in nix/) should ensure the cache is updated before it’s merged into trunk – to avoid there being any time when contributors end up rebuilding the tooling locally.

The easiest way to do this is to manually run the [Nix development cache](https://github.com/unisonweb/unison/actions/workflows/nix-dev-cache.yaml) workflow on the PR’s branch (which may be located on your fork).

Depending on how much needs to be rebuilt, some or all of the jobs may fail. In this case, you can populate the cache from another machine[^1].

``` bash
nix build .#all --accept-flake-config --json --keep-going --system ${system} \
  | jq --raw-output '.[].outputs | to_entries[].value' \
  | cachix --verbose push unison
```
(adapted from [the Cachix documentation](https://docs.cachix.org/pushing#pushing-runtime-closure))

`system` is generally the Nix system (e.g., `x86_64-linux`, `aarch64-darwin`) you are running this on, but you can also use different systems in some cases. E.g.,
if you are running on an Apple silicon Mac, you can also run the above command using `--system x86_64-darwin` to cache Intel Mac derivations.

Another issue @sellout has run into is that `cachix` seems to crash a lot on at least some Macs. To keep re-trying until it succeeds, try

``` bash
until
  nix build .#all --accept-flake-config --json --keep-going \
    | jq --raw-output '.[].outputs | to_entries[].value' \
    | cachix --verbose push unison
do :
done
```

### updating after a release

After a release tag is made, we additionally need to [pin](https://unison.cachix.org#pins) the new releases in the cache, so they don’t expire.

```bash
cachix pin unison "ucm-${version}-${system}" "/nix/store/${hash}-unison-cli-main-exe-unison-0.0.0"
```

__NB__: Since the Cabal files don’t have the version set, the version in the Nix store will always be “0.0.0”, regardless of the actual release.
