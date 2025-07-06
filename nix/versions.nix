{lib ? (import <nixpkgs> {}).lib}: let
  ## It’s much easier to read from a JSON file than to have JSON import from some other file, so we extract some
  ## configuration from the VS Code settings to avoid duplication.
  vscodeSettings = lib.importJSON ../.vscode/settings.json;
in
  vscodeSettings."haskell.toolchain"
  ## There are some things we want to pin that the VS Code Haskell extension doesn’t let us control.
  // {
    hpack = "0.36.0"; # This needs to match the version returned by `nix develop --command stack --version`.
    ormolu = "0.7.2.0";
  }
