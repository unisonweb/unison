These are commands that will likely be useful during development.

__General:__ `./scripts/test.sh` compiles and builds the Haskell code and runs all tests. Recommended that you run this before pushing any code to a branch that others might be working on.

_Disclaimer_ If you have trouble getting started, please get in touch via [Discord](https://unison-lang.org/discord) so we can help.  If you have any fixes to the process, please send us a PR!

## Development Dependencies

If you are having trouble with a build, please ensure that your tooling matches the versions we expect. Some build mechanisms will guarantee this to some extent (e.g., the Nix build, or [the Haskell extension for VS Code](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)), but you can see versions listed in [our version file](./nix/versions.nix) (some of which is inherited from [this repo’s VS Code settings](./.vscode/settings.json)).

## Running Unison

To get cracking with Unison:

1. [Install `stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install).
2. Build the project with `stack build`. This builds all executables.
3. (Optional) Run `./dev-ui-install.hs` to fetch the latest release of the codebase UI. If you don't care about running the codebase UI locally you can ignore this step.
4. After building do `stack exec unison` to will initialize a codebase in your home directory (in `~/.unison`). This only needs to be done once. (Alternatively, you can use `stack exec -- unison -C <other dir> to create a codebase in <other dir>`
5. `stack exec unison` starts Unison and watches for `.u` file changes in the current directory. If you want to run it in a different directory, just add `unison` to your `PATH`, after finding it with `stack exec which unison`.

On startup, Unison prints a url for the codebase UI. If you did step 3 above, then visiting that URL in a browser will give you a nice interface to your codebase.

## Git hooks

There are some Git hooks provided by Unison. If you want to use them, you can run

``` bash
./scripts/install-hooks.bash
```

## Autoformatting your code with Ormolu

We use Ormolu (see [the specific version](./nix/versions.nix)) and CI will add an extra commit, if needed, to autoformat your code.

Also note that you can always wrap a comment around some code you don't want Ormolu to touch, using:

```haskell
{- ORMOLU_DISABLE -}
{- because we carefully formatted this code for readability -}
dontFormatMe = do blah
                    blah
                  blah
{- ORMOLU_ENABLE -}
```
__NB__: Always include an extra comment (as above) to explain _why_ you’re disabling Ormolu.

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

## Building with Cabal

Unison can also be built/installed with Cabal. You'll need the same ghc
used by `stack.yaml` to successfully build its dependencies.
The provided project file is also in contrib/ so you'll need to specify
its location on the command line.

* To build all projects use

    `cabal build --project-file=contrib/cabal.project all`

* Tests can be run with e.g.

    `cabal test --project-file=contrib/cabal.project all`

* The executable can be installed with

    `cabal install --project-file=contrib/cabal.project unison`

* The install directory can be modified with the option `--installdir=`

* Take in account that if you want to load the project in haskell-language-server using cabal instead stack you will need:
  * Symlink `contrib/cabal.project*` to the project root (for example, `ln -s contrib/cabal.project* ./`)
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

## Nix support

See the [readme](./nix/README.md).

## Updating the Toolchain

This doesn’t need to all be done together, but it’s presented that way for the simplest overview of the entire process.

### update the Nix flake

- Update `inputs.nixpkgs.url` in [flake.nix](./flake.nix) – it should generally point to the latest release, but because of how the Haskell integration is managed, the `nixpkgs-unstable` branch is a good way to get fixed Haskell packages & a newer Stackage LTS.
- Run `nix flake update` to pull the latest commits (even if you didn’t update the Nixpkgs URL).

### iterate over the errors induced by this change

Run `nix develop --command stack --version`, which needs to succeed to allow for Stack-based development within Nix. It also works through a bunch of the dependencies without yet trying to build the Unison Haskell code.

Once this command succeeds, you’ll be in pretty good shape, but it’ll error based on what’s changed in Nixpkgs. For example, it will cause you to update

- versions listed in [settings.yaml](./.vscode/settings.yaml) and [versions.nix](./nix/versions.nix) to the ones included in the new Nixpkgs,
- the `resolver` entry in [stack.yaml](./stack.yaml), and
- Haskell packages missing from Stackage.

### update Stack configuration

- Update the `resolver` in [stack.yaml](./stack.yaml) – it should be lts-XX.YY, where the number matches the entry from [Stackage](https://www.stackage.org/) for the GHC we want to use (**NB**: I recommend choosing the Stackage LTS that matches the one used for `nixpkgs.legacyPackages.${system}.haskellPackages`, because it gives the most alignment with the minimum manual configuration – it’s the one listed in [this Nixpkgs file](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-hackage2nix/stackage.yaml), but make sure to get the version from the Nixpkgs branch [our flake](./flake.nix) is using).

- Comment out  `extra-deps` in [stack.yaml](./stack.yaml) (to see if they’re no longer necessary, but easy to restore if they are). When restoring any `extra-deps`, it‘s a good idea to see if there’s a more current version to pull – this can be especially helpful if it otherwise needs `allow-newer-deps` or other workarounds to work with the updated LTS.

### get Unison code building

- Run `nix develop --command stack test` or your preferred Stack-based build commands.

This can be anywhere from trivial to a mighty slog, depending on Haskell packages that have changed, GHC changes, etc. You might have to update stack.yaml to add new `extra-deps`, etc.

If you need to add additional `extra-deps`, first add a simple `- package-version` entry, then after running `stack`, look for the reference in [stack.yaml.lock](./stack.yaml.lock) and copy the full `sha256` entry to [stack.yaml](./stack.yaml).

### update the Nix configuration

Edit [unison-project.nix](./nix/unison-project.nix)

- `baseHaskellPkgSet` should match the GHC version from the Stackage LTS you’re using (this should already be the case if you set the Stackage LTS to one used for Nixpkgs’ `haskellPackages` package set).
- Ensure the section referencing cdepillabout/stacklock2nix#54 matches the `allow-newer-deps` section from [stack.yaml](./stack.yaml) (this step should be removed if that issue is fixed).
- optional: comment out the `dontCheck` lines to see if updated versions now work fine in the sandbox.
- Run `nix build`

This is very similar to the Stack command (the Nix configuration is extracted from the Stack configuration), but there are some other things that can go wrong (for example, if a package’s `test-suite`s fail in the Nix sandbox, you’ll need to add a `dontCheck` entry for that package).

### update other tooling

Even though everything builds, there are other things in the repo (for example, GitHub actions) that reference some of the same tools, so the versions should match.

For each of the entries in  [settings.yaml](./.vscode/settings.yaml) and [versions.nix](./nix/versions.nix) that changed, search the codebase for the old version number, replacing it with the new one.

**NB**: If you updated hpack, `rm **/*.cabal` then restore yaks/easytest/easytest.cabal. Then `stack build` will regenerate all of the Cabal files using the new hpack version.

**NB**: If you updated Ormolu, also update the version mentioned in [scripts/check-formatting](./scripts/check-formatting) then run that script to 1. make sure it works and 2. bring the code in sync with the new version.

**NB**: If you updated Weeder, also update the version mentioned in [scripts/check-weeds](./scripts/check-weeds) then run that script to 1. make sure it works and 2. bring the weed list in sync with the new version ([§Weeding](#weeding)).

### update Cabal configuration

**NB**: Cabal isn’t officially supported, but these couple steps should keep it working.

1. Copy https://www.stackage.org/lts-XX.YY/cabal.config (for the correct XX.YY) to [cabal.project.freeze](./contrib/cabal.project.freeze) (search the dif for `-- ` to find any local changes we may have made to the file that need to be preserved). This keeps Cabal users in sync with Stack users.
2. Replicate the changes made to [stack.yaml](./stack.yaml) into [cabal.project](./contrib/cabal.project).
3. Run `cabal build all` and cross your fingers.

### update the Nix cache

[docs on updating the cache](./nix/README.md#updating-when-the-development-environment-changes)

## Weeding

The Nix devShell includes Weeder, a tool for detecting dead code.

Before running it, make sure your build is up-to-date (`stack build --bench --no-run-benchmarks --test --no-run-tests --haddock`).

Not building tests will result in false reports of dead code, because Weeder won’t see call sites of things used only by tests. However, if Weeder runs clean on the entire codebase, you can `stack clean && stack build && weeder` and the complaints Weeder emits will then be things that are defined in production code, but are only used in benchmarks, tests, or Haddock. This indicates either dead code that we’re testing, or test utilities that live in the wrong place. The former should be removed, and the later should be moved.

__NB__: Sometimes weeder complains about HIE files being built with the wrong GHC version. To fix this, I’ve had success with deleting my .direnv cache. You can specify multiple directories for Weeder to search with `--hie-directory`, but can’t specify a directory to exclude.
