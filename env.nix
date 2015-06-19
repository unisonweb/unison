let
  defaultPkgs = import <nixpkgs> {};

  unison-src = defaultPkgs.fetchgitLocal ./.;

  # Should be a fixpoint, right now clobbers other `packages.*`. see nixpkgs #7659
  extender = nixpkgs: let

    importer = self: super: dir: {
      name = "unison-${dir}";
      value = let
        src = unison-src + "/${dir}";
        inputs = { inherit (nixpkgs) stdenv; } // self;
        vanilla = nixpkgs.stdenv.lib.callPackageWith inputs src {};
        in nixpkgs.haskell-ng.lib.overrideCabal vanilla (_: { inherit src; });
    };

    overrider = dirs: {
      overrides = self: super: with builtins;
        listToAttrs (map (importer self super) dirs);
    };

    brokenOverrideHack = super: dirs: let
      self = super // (overrider dirs).overrides self super;
    in self;

  in (nixpkgs // {
    # Should be a fixpoint so we can replace original. see nixpkgs #7659
    unisonPackages.ghc7101 = brokenOverrideHack nixpkgs.reflexPackages.ghc7101
      [ "editor" "shared" "node" ];
    unisonPackages.ghcjs = brokenOverrideHack nixpkgs.reflexPackages.ghcjs
      [ "editor" "shared" ];
  });

  try-reflex = defaultPkgs.fetchFromGitHub {
    owner = "unisonweb";
    repo = "try-reflex";
    rev = "663b9bef61f4b87f9c3183d28f0efd1be9b91ae8";
    sha256 = "1bi3ndxyv57f58xxazmln1dyg5agp1pxyinh6n7hn7x6kh9czcp2";
  };

  reflexPkgs = import "${try-reflex}/deps" {};

in extender reflexPkgs
