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
    owner = "unisonweb";
    repo = "try-reflex";
    rev = "ba7437c89ae6f8d9f9267f3e50eac455911a2371";
    sha256 = "0gpr0203ms1qsxi07mg78c36xwsynlf9ib8k3jb6i3v1k5ggpvr5";
  };

  reflexPkgs = import "${try-reflex}/deps" {};

in extender reflexPkgs
