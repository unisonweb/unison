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
    ghc7101 = brokenOverrideHack reflexPkgs.ghc [ "editor" "shared" "node" ];
    ghcjs = brokenOverrideHack reflexPkgs.ghcjs [ "editor" "shared" ];
  };

  unison-dependencies = localPkgs.fetchgit {
    url = git://github.com/ryantrinkle/try-reflex;
    rev = "e24d4e7d2a37a770883749e10cad787662f6653f";
    sha256 = "0xrbb0r583s4mkjqi49m2wqjqvscl1sis1k5j5yn7nr8887429z3";
  };

in makeUnisonPkgSet (import "${unison-dependencies}" {})
