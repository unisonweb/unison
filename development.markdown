These are commands that will likely be useful during development.

__General:__ `./scripts/test.sh` compiles and builds the Haskell code and runs all tests. Recommended that you run this before pushing any code to a branch that others might be working on.

_Disclaimer_ If you have trouble getting started, please get in touch via [Discord](https://unison-lang.org/discord) so we can help.  If you have any fixes to the process, please send us a PR!

## Running Unison

To get cracking with Unison:

1. [Install `stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install).
2. Build the project with `stack build`. This builds all executables.
3. (Optional) Run `./dev-ui-install.hs` to fetch the latest release of the codebase UI. If you don't care about running the codebase UI locally you can ignore this step.
4. After building do `stack exec unison` to will initialize a codebase in your home directory (in `~/.unison`). This only needs to be done once. (Alternatively, you can use `stack exec -- unison -C <other dir> to create a codebase in <other dir>`
5. `stack exec unison` starts Unison and watches for `.u` file changes in the current directory. If you want to run it in a different directory, just add `unison` to your `PATH`, after finding it with `stack exec which unison`.

On startup, Unison prints a url for the codebase UI. If you did step 3 above, then visiting that URL in a browser will give you a nice interface to your codebase.

## Autoformatting your code with Ormolu

We use 0.5.0.1 of Ormolu and CI will add an extra commit, if needed, to autoformat your code.

Also note that you can always wrap a comment around some code you don't want Ormolu to touch, using:

```haskell
{- ORMOLU_DISABLE -}
dontFormatMe = do blah
                    blah
                  blah
{- ORMOLU_ENABLE -}
```

## Running Tests

* `stack test --fast` builds and runs most test suites, see below for exceptions to this (e.g. transcript tests).

Most test suites support selecting a specific test to run by passing a prefix as a test argument:

* `stack test unison-parser-typechecker --fast --test-arguments my-test-prefix` builds and runs most test suites, see below for exceptions to this (e.g. transcript tests).

Some tests are executables instead:

* `stack exec transcripts` runs the transcripts-related integration tests, found in `unison-src/transcripts`. You can add more tests to this directory.
* `stack exec transcripts -- prefix-of-filename` runs only transcript tests with a matching filename prefix.
* `stack exec cli-integration-tests` runs the additional integration tests for cli. These tests are not triggered by `tests` or `transcripts`.
* `stack exec unison -- transcript unison-src/transcripts-round-trip/main.md` runs the pretty-printing round trip tests
* `stack exec unison -- transcript unison-src/transcripts-manual/benchmarks.md` runs the benchmark suite. Output goes in unison-src/transcripts-manual/benchmarks/output.txt.

### Building everything at once, including tests and benchmarks, but without running them:
Do:

    stack build --fast --test --bench --no-run-tests --no-run-benchmarks

### What if you want a profiled build?

Do:

    stack build --profile unison-parser-typechecker

Again you can leave off the flag. To run an executable with profiling enabled, do:

    stack exec -- <executable-name> +RTS -p

That will generate a `<executable-name>.prof` plain text file with profiling data. [More info on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).

## Building with cabal

Unison can also be built/installed with cabal. You'll need the same ghc
used by `stack.yaml` to successfully build its dependencies.
The provided project file is also in contrib/ so you'll need to specify
its location on the command line.

* To build all projects use

    `cabal v2-build --project-file=contrib/cabal.project all`

* Tests can be run with e.g.

    `cabal v2-test --project-file=contrib/cabal.project all`

* The executable can be installed with

    `cabal v2-install --project-file=contrib/cabal.project unison`

* The install directory can be modified with the option `--installdir: ...`

* Take in account that if you want to load the project in haskell-language-server using cabal instead stack you will need:
  * Copy or link `./contrib/cabal.project` to `./cabal.project`
  * Delete or rename the existing `./hie.yaml`. The default behaviour without `hie.yaml` works with cabal.

## Building on Windows

### I get an error about unison/sql/something

This codebase uses symlinks as a workaround for some inconveniences in the `here` package. Support for symlinks in Windows is relatively new, and isn't enabled by default. As a result, your cloned copy of the code probably won't build.

First you'll need to enable "Developer Mode" in your Windows settings.

> See https://consumer.huawei.com/en/support/content/en-us15594140/

Then you'll need to enable symlink support in your `git` configuration, e.g.

```shell
git config core.symlinks true
```

And then ask `git` to fix up your symlinks with `git checkout .`

More context at: https://stackoverflow.com/a/59761201/310162


### I get an error about `removeDirectoryRecursive`/`removeContentsRecursive`/`removePathRecursive`/`permission denied (Access is denied.)`

Stack doesn't work deterministically in Windows due to mismatched expectations about how file deletion works. If you get this error, you can just retry the build and it will probably make more progress than the last time.

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

## Native compilation

See the [readme](scheme-libs/racket/unison/Readme.md).
