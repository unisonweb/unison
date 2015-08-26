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
    rev = "8eb31fef1969a4298ae7c456713bf31085ade68b";
    sha256 = "1vr6bxq3fbwwfz46vild29qvjrdfk6za0pcji9vbai7gdjm4j82z";
  };

in makeUnisonPkgSet (import "${unison-dependencies}" {})
