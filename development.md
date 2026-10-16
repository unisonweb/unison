# Developing Unison

How to build, run, test and check the code in this repository. If you have trouble getting started, please get in touch via [Discord](https://unison-lang.org/discord) so we can help, and if you have fixes to the process, please send a PR.

The short version: build with `stack build`, run with `stack exec unison`, and before pushing run `./scripts/check.sh`, which runs every check CI will ask for and records that you did ([Checks before pushing](#checks-before-pushing)).

## Dependencies

[Install `stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install). If you are having trouble with a build, please ensure that your tooling matches the versions we expect. Some build mechanisms guarantee this to some extent (the Nix build, or [the Haskell extension for VS Code](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)); the versions are listed in [our version file](./nix/versions.nix) (some of which is inherited from [this repo's VS Code settings](./.vscode/settings.json)).

Other ways to build: [with Nix](./nix/README.md), [with Cabal](./docs/building-with-cabal.md) (not officially supported), and [on Windows](./docs/building-on-windows.md), which has two known traps.

## Building and running

1. `stack build` builds all executables. `stack build --fast` is quicker and is what the checks use; `stack build --fast --test --bench --no-run-tests --no-run-benchmarks` builds everything, tests and benchmarks included, without running them.
2. (Optional) Run `./dev-ui-install.hs` to fetch the latest release of the codebase UI. If you don't care about running the codebase UI locally you can skip this.
3. `stack exec unison` starts Unison on a codebase in your home directory (`~/.unison`), creating it the first time, and watches for `.u` file changes in the current directory. `stack exec -- unison -C <dir>` uses or creates a codebase in `<dir>` instead. To run it from another directory, add the executable's directory (`stack exec which unison`) to your `PATH`.

On startup, Unison prints a URL for the codebase UI. If you did step 2, visiting that URL in a browser gives you an interface to your codebase.

## Running tests

* `stack test --fast` builds and runs most test suites. Most suites take a prefix to select tests: `stack test unison-parser-typechecker --fast --test-arguments my-test-prefix`.
* `stack exec transcripts` runs the transcript tests in `unison-src/transcripts`; `stack exec transcripts -- prefix-of-filename` runs only those whose file name starts with the prefix. Add a `.md` file to that directory to add a test.
* `stack exec cli-integration-tests` runs the CLI integration tests, which neither of the above runs.
* `stack exec unison -- transcript unison-src/transcripts-round-trip/main.md` runs the pretty-printing round-trip tests.
* `stack exec unison -- transcript unison-src/transcripts-manual/benchmarks.md` runs the benchmark suite; output goes in `unison-src/transcripts-manual/benchmarks/output.txt`.

`./scripts/check.sh` runs the tests, transcripts and integration tests the way CI does, and the formatting and migration checks besides; see the next section.

### Profiled builds

`stack build --profile unison-parser-typechecker` (or without the package, for everything), then run an executable with `stack exec -- <executable-name> +RTS -p`. That writes a `<executable-name>.prof` text file with the profiling data. [More on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).

## Checks before pushing

CI does not rerun the expensive checks on every push. Instead each check has an *attestation*: a hash of the files it depends on, plus the verdict, recorded in `.github/workflows/proofs/<check>.txt`. When you run a check locally it writes its attestation; you commit that file with your change; and CI only verifies that the committed attestation matches the commit's files. If it doesn't, CI fails and tells you which script to run. The merge queue runs everything regardless, so an attestation is a shortcut for pull requests, not a substitute for the full run.

`./scripts/check.sh` runs all of them:

| Check | Script | What it runs | Inputs hashed |
| --- | --- | --- | --- |
| tests | `scripts/proofs/tests.sh` | `stack build --fast --test` and the CLI integration tests | Haskell and C sources, build config, integration test inputs |
| transcripts | `scripts/proofs/transcripts.sh` | every transcript, the round-trip and docs transcripts | Haskell and C sources, build config, `unison-src` |
| formatting | `scripts/proofs/formatting.sh` | `ormolu --check-idempotence` (see [Formatting](#formatting)) | Haskell sources, `.ormolu` |
| migration | `scripts/proofs/migration.sh` | `scripts/migration-test.sh`: the latest release makes a codebase, this ucm must migrate it | the migrations, the schema version, the latest release's tag |

Things worth knowing:

- A check whose attestation already covers the current files says `cached (pass)` and does nothing; `--force` reruns it. `--dry-run` says what would run, `--verbose` shows the hashes and where they were found.
- The hashes of the tests, transcripts and migration checks ignore comments and whitespace in Haskell and C sources, so a comment edit or a reformat doesn't invalidate them. The formatting check hashes the raw text.
- The migration check needs network access (it downloads the latest release from GitHub and installs `@unison/base` from Share), and its attestation includes the release it was run against, recorded in `.github/workflows/proofs/migration-release.txt`. A new release invalidates it until someone runs it again.
- If you are sure a change can only affect some transcripts, `./scripts/proofs/transcripts.sh --only PREFIX` runs just the transcripts whose file names start with `PREFIX` (repeatable; `round-trip`, `docs.to-html` and `fix5507` name the manual ones) and records a *partial* attestation, with the transcripts run and their hashes in `.github/workflows/proofs/transcripts-partial.txt`. Pull request CI accepts it with a notice; the merge queue runs all transcripts; a later full run replaces it.
- The [pre-push hook](#git-hooks) checks the attestations of the commits you push and runs any that are missing, amending the proofs files into the commit, so with the hook installed you rarely run the scripts by hand.
- To have CI do the full run for a pull request and push the attestations to its branch, a maintainer comments `🤖 run full verification` on it. The bot reacts to the comment, runs `./scripts/check.sh --force`, commits the proofs files, pushes, and comments the outcome. It can push to pull requests from forks only when the repository has a `FULL_VERIFICATION_TOKEN` secret.

## Git hooks

The repository provides a pre-commit hook and the pre-push hook described above. Install them with

``` bash
./scripts/hooks/install.bash
```

Each installed hook tells you when it differs from the repository's copy; `./scripts/hooks/install.bash -f` updates them.

## Formatting

We use Ormolu (the version is in [nix/versions.nix](./nix/versions.nix) and [scripts/check-formatting](./scripts/check-formatting)). `./scripts/check-formatting` checks every file, and `./scripts/check-formatting inplace` reformats. On a pull request from a branch in this repository, CI adds a commit that reformats the code if needed; on a pull request from a fork it only checks, so format before pushing.

You can wrap a comment around code you don't want Ormolu to touch:

```haskell
{- ORMOLU_DISABLE -}
{- because we carefully formatted this code for readability -}
dontFormatMe = do blah
                    blah
                  blah
{- ORMOLU_ENABLE -}
```
__NB__: Always include an extra comment (as above) to explain _why_ you're disabling Ormolu.

## Weeding

Weeder detects dead code; the Nix devShell includes it, and `./scripts/check-weeds` runs it. It has an attestation script too, `scripts/proofs/weeds.sh`, but `check.sh` doesn't run it, since the weed list has known failures.

Before running it, make sure your build is up to date (`stack build --bench --no-run-benchmarks --test --no-run-tests --haddock`). Not building tests results in false reports of dead code, because Weeder won't see call sites of things used only by tests. However, if Weeder runs clean on the entire codebase, you can `stack clean && stack build && weeder` and the complaints Weeder then emits are things defined in production code but only used in benchmarks, tests or Haddock: either dead code that we're testing, or test utilities that live in the wrong place. The former should be removed and the latter moved.

__NB__: Sometimes Weeder complains about HIE files being built with the wrong GHC version. Deleting the `.direnv` cache has fixed this. You can specify multiple directories for Weeder to search with `--hie-directory`, but can't specify a directory to exclude.

## Updating the toolchain

Bumping GHC, Stackage, Nixpkgs, Ormolu, Weeder or hpack touches several files that have to agree. The procedure is in [docs/updating-the-toolchain.md](./docs/updating-the-toolchain.md).
