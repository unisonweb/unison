{
  description = "Unison";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            unison-project = with prev.lib.strings;
              let
                cleanSource = pth:
                  let
                    src' = prev.lib.cleanSourceWith {
                      filter = filt;
                      src = pth;
                    };
                    filt = path: type:
                      let
                        bn = baseNameOf path;
                        isHiddenFile = hasPrefix "." bn;
                        isFlakeLock = bn == "flake.lock";
                        isNix = hasSuffix ".nix" bn;
                      in !isHiddenFile && !isFlakeLock && !isNix;
                  in src';
              in final.haskell-nix.project' {
                src = cleanSource ./.;
                compiler-nix-name = "ghc928";
                projectFileName = "stack.yaml";
                shell = {
                  buildInputs = with pkgs; [ ];
                  tools = let ormolu-ver = "0.5.2.0";
                  in {
                    cabal = { };
                    ormolu = { version = ormolu-ver; };
                    haskell-language-server = {
                      version = "latest";
                      # specify flags via project file rather than a module override
                      # https://github.com/input-output-hk/haskell.nix/issues/1509
                      cabalProject = ''
                        packages: .
                        package haskell-language-server
                          flags: -brittany -fourmolu -stylishhaskell -hlint
                        constraints: ormolu == ${ormolu-ver}
                      '';
                    };
                  };
                };
                branchMap = {
                  "https://github.com/unisonweb/configurator.git"."e47e9e9fe1f576f8c835183b9def52d73c01327a" =
                    "unison";
                  "https://github.com/unisonweb/shellmet.git"."2fd348592c8f51bb4c0ca6ba4bc8e38668913746" =
                    "topic/avoid-callCommand";
                };
              };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.unison-project.flake { };
      in flake // { defaultPackage = flake.packages."unison-cli:exe:unison"; });
}
