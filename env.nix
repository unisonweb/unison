let
  defaultPkgs = import <nixpkgs> {};

  # Should be a fixpoint, right now clobbers other `packages.*`. see nixpkgs #7659
  extender = nixpkgs: let

    importer = self: super: dir: {
      name = "unison-${dir}";
      value = let
        src = defaultPkgs.fetchgitLocal (./. + "/${dir}");
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

  unison-dependencies = defaultPkgs.fetchFromGitHub {
    owner = "unisonweb";
    repo = "unison-dependencies";
    rev = "0882249d91daff85d9d9656802b65d6408eaf5c2";
    sha256 = "1p9fxkkir5n4w8xhpqnq29ffymyvbsp8b8ali735vvk5nmlp47fz";
  };

in extender (import "${unison-dependencies}/deps" {})
