## Building with Nix

__NB__: It is important that the Unison Nix cache is trusted when building, otherwise you will likely end up building hundreds of packages, including GHC itself.

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

It is _not_ recommended to add your user to `trusted-users`. This _can_ make enabling flake configurations simpler (like the Unison Nix cache here), but [it is equivalent to giving that user root access (without need for sudo)](https://nix.dev/manual/nix/2.23/command-ref/conf-file.html#conf-trusted-users).

## Building package components with nix

### Build the unison executable
```shell
nix build
```

### Build a specific component
This is specified with the normal
`<package>:<component-type>:<component-name>` triple.

Some examples:
```shell
nix build '.#component-unison-cli:lib:unison-cli'
nix build '.#component-unison-syntax:test:syntax-tests'
nix build '.#component-unison-cli:exe:transcripts'
```

### Development environments

#### Get into a development environment for building with stack
This gets you into a development environment with the preferred
versions of the compiler and other development tools. These
include:

- ghc
- stack
- ormolu
- haskell-language-server

```shell
nix develop
```

#### Get into a development environment for building with cabal
This gets you into a development environment with the preferred
versions of the compiler and other development tools. Additionally,
all non-local haskell dependencies (including profiling dependencies)
are provided in the nix shell.

```shell
nix develop '.#cabal-local'
```

#### Get into a development environment for building a specific package
This gets you into a development environment with the preferred
versions of the compiler and other development tools. Additionally,
all haskell dependencies of this package are provided by the nix shell
(including profiling dependencies).

```shell
nix develop '.#cabal-<package-name>'
```

for example:

```shell
nix develop '.#cabal-unison-cli'
```
or
```shell
nix develop '.#cabal-unison-parser-typechecker'
```

This is useful if you wanted to profile a package. For example, if you
want to profile `unison-cli-main:exe:unison` then you could get into one of these
shells, cd into its directory, then run the program with
profiling.

```shell
nix develop '.#cabal-unison-parser-typechecker'
cd unison-cli
cabal run --enable-profiling unison-cli-main:exe:unison -- +RTS -p
```
