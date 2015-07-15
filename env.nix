let
  localPkgs = import <nixpkgs> {};

  makeUnisonPkgSet = reflexPkgs: let

    importer = self: super: dir: {
      name = "unison-${dir}";
      value = let
        src = localPkgs.fetchgitLocal (./. + "/${dir}");
        inputs = { inherit (reflexPkgs.nixpkgs) stdenv; } // self;
        vanilla = reflexPkgs.nixpkgs.stdenv.lib.callPackageWith inputs src {};
        in reflexPkgs.nixpkgs.haskell-ng.lib.overrideCabal vanilla (_: { inherit src; });
    };

    overrider = dirs: {
      overrides = self: super: with builtins;
        listToAttrs (map (importer self super) dirs);
    };

    brokenOverrideHack = super: dirs: let
      self = super // (overrider dirs).overrides self super;
    in self;

  in {
    inherit (reflexPkgs) nixpkgs;
    ghc7101 = brokenOverrideHack reflexPkgs.ghc7101 [ "editor" "shared" "node" ];
    ghcjs = brokenOverrideHack reflexPkgs.ghcjs [ "editor" "shared" ];
  };

  unison-dependencies = localPkgs.fetchFromGitHub {
    owner = "unisonweb";
    repo = "unison-dependencies";
    rev = "80048a76ff35d4ed9f267be7076bec83a193a71f";
    sha256 = "0cvx1pibbph0lykvd2wwagwgphf8jifmvg405mkxpsxf6nap26wc";
  };

in makeUnisonPkgSet (import "${unison-dependencies}/deps" {})
