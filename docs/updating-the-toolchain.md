# Updating the toolchain

Moved here from `development.md`. This doesn't need to all be done together, but it's presented that way for the simplest overview of the entire process.

## Update the Nix flake

- Update `inputs.nixpkgs.url` in [flake.nix](../flake.nix): it should generally point to the latest release, but because of how the Haskell integration is managed, the `nixpkgs-unstable` branch is a good way to get fixed Haskell packages and a newer Stackage LTS.
- Run `nix flake update` to pull the latest commits (even if you didn't update the Nixpkgs URL).

## Iterate over the errors induced by this change

Run `nix develop --command stack --version`, which needs to succeed to allow for Stack-based development within Nix. It also works through a bunch of the dependencies without yet trying to build the Unison Haskell code.

Once this command succeeds, you'll be in pretty good shape, but it'll error based on what's changed in Nixpkgs. For example, it will cause you to update

- versions listed in [settings.yaml](../.vscode/settings.json) and [versions.nix](../nix/versions.nix) to the ones included in the new Nixpkgs,
- the `resolver` entry in [stack.yaml](../stack.yaml), and
- Haskell packages missing from Stackage.

## Update Stack configuration

- Update the `resolver` in [stack.yaml](../stack.yaml): it should be lts-XX.YY, where the number matches the entry from [Stackage](https://www.stackage.org/) for the GHC we want to use (**NB**: choose the Stackage LTS that matches the one used for `nixpkgs.legacyPackages.${system}.haskellPackages`, because it gives the most alignment with the minimum manual configuration; it's the one listed in [this Nixpkgs file](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-hackage2nix/stackage.yaml), but make sure to get the version from the Nixpkgs branch [our flake](../flake.nix) is using).
- Comment out `extra-deps` in [stack.yaml](../stack.yaml) (to see if they're no longer necessary, but easy to restore if they are). When restoring any `extra-deps`, it's a good idea to see if there's a more current version to pull; this can be especially helpful if it otherwise needs `allow-newer-deps` or other workarounds to work with the updated LTS.

## Get Unison code building

- Run `nix develop --command stack test` or your preferred Stack-based build commands.

This can be anywhere from trivial to a mighty slog, depending on Haskell packages that have changed, GHC changes, etc. You might have to update stack.yaml to add new `extra-deps`, etc.

If you need to add additional `extra-deps`, first add a simple `- package-version` entry, then after running `stack`, look for the reference in [stack.yaml.lock](../stack.yaml.lock) and copy the full `sha256` entry to [stack.yaml](../stack.yaml).

## Update the Nix configuration

Edit [unison-project.nix](../nix/unison-project.nix)

- `baseHaskellPkgSet` should match the GHC version from the Stackage LTS you're using (this should already be the case if you set the Stackage LTS to one used for Nixpkgs' `haskellPackages` package set).
- Ensure the section referencing cdepillabout/stacklock2nix#54 matches the `allow-newer-deps` section from [stack.yaml](../stack.yaml) (this step should be removed if that issue is fixed).
- optional: comment out the `dontCheck` lines to see if updated versions now work fine in the sandbox.
- Run `nix build`

This is very similar to the Stack command (the Nix configuration is extracted from the Stack configuration), but there are some other things that can go wrong (for example, if a package's `test-suite`s fail in the Nix sandbox, you'll need to add a `dontCheck` entry for that package).

## Update other tooling

Even though everything builds, there are other things in the repo (for example, GitHub actions) that reference some of the same tools, so the versions should match.

For each of the entries in [settings.yaml](../.vscode/settings.json) and [versions.nix](../nix/versions.nix) that changed, search the codebase for the old version number, replacing it with the new one.

**NB**: If you updated hpack, `rm **/*.cabal` then restore yaks/easytest/easytest.cabal. Then `stack build` will regenerate all of the Cabal files using the new hpack version.

**NB**: If you updated Ormolu, also update the version mentioned in [scripts/check-formatting](../scripts/check-formatting) and in `.github/workflows/format.yaml` and `.github/workflows/full-verification.yaml`, then run the script to 1. make sure it works and 2. bring the code in sync with the new version.

**NB**: If you updated Weeder, also update the version mentioned in [scripts/check-weeds](../scripts/check-weeds) then run that script to 1. make sure it works and 2. bring the weed list in sync with the new version (see "Weeding" in `development.md`).

## Update Cabal configuration

**NB**: Cabal isn't officially supported, but these couple steps should keep it working.

1. Copy https://www.stackage.org/lts-XX.YY/cabal.config (for the correct XX.YY) to [cabal.project.freeze](../contrib/cabal.project.freeze) (search the diff for `-- ` to find any local changes we may have made to the file that need to be preserved). This keeps Cabal users in sync with Stack users.
2. Replicate the changes made to [stack.yaml](../stack.yaml) into [cabal.project](../contrib/cabal.project).
3. Run `cabal build all` and cross your fingers.

## Update the Nix cache

[docs on updating the cache](../nix/README.md#updating-when-the-development-environment-changes)
