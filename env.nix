let
  defaultPkgs = import <nixpkgs> {};

  unison-src = defaultPkgs.fetchgitLocal ./.;

  # Should be a fixpoint, right now clobbers other `packages.*`. see nixpkgs #7659
  extender = nixpkgs: let

    importer = self: super: dir: {
      name = "unison-${dir}";
      value = let
        src = unison-src + "/${dir}";
        vanilla = self.callPackage src {};
        in nixpkgs.haskell-ng.lib.overrideCabal vanilla (_: { inherit src; });
    };

    overrider = dirs: {
      overrides = self: super: with builtins;
        listToAttrs (map (importer self super) dirs);
    };

  in (nixpkgs // {
    # Should be a fixpoint so we can replace original. see nixpkgs #7659
    unisonPackages.ghc7101 = nixpkgs.reflexPackages.ghc7101.override
      (overrider [ "editor" "shared" "node" ]);
    unisonPackages.ghcjs = nixpkgs.reflexPackages.ghcjs.override
      (overrider [ "editor" "shared" ]);
  });

  try-reflex = defaultPkgs.fetchFromGitHub {
    owner = "Ericson2314";
    repo = "try-reflex";
    rev = "16d45d152d9110a4da696f2596954b3b0516b93f";
    sha256 = "0pv4mm2nf5630dvnzxxm9anijgxm3bn2a185klfskrnqk9mgfjga";
  };

  reflexPkgs = import "${try-reflex}/deps" {};

in extender reflexPkgs
