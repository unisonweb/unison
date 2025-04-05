{nixpkgs-release}: final: prev: let
  pinned-pkgs = import nixpkgs-release {inherit (final) system;};
in {
  stack = pinned-pkgs.stack;

  ## See https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers for an
  ## explanation of this package.
  stack-wrapped = final.symlinkJoin {
    name = "stack"; # will be available as the usual `stack` in terminal
    paths = [final.stack];
    buildInputs = [final.makeWrapper];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --no-nix \
          --system-ghc \
          --no-install-ghc \
        "
    '';
  };
}
