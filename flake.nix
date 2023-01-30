{
  description = "A common environment for unison development";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = { self, flake-utils, nixpkgs }:
    let
      systemAttrs = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages."${system}".extend self.overlay;

          mystack = pkgs.stack;
          ghc-version = "8107";
          ghc = pkgs.haskell.packages."ghc${ghc-version}";
          make-ormolu = p:
            p.callHackageDirect {
              pkg = "ormolu";
              ver = "0.4.0.0";
              sha256 = "0r8jb8lpaxx7wxnvxiynx2dkrfibfl8nxnjl5n4vwy0az166bbnd";
            } {
              ghc-lib-parser =
                pkgs.haskellPackages.ghc-lib-parser_9_2_5_20221107;
              Cabal = pkgs.haskellPackages.Cabal_3_6_3_0;
            };
          myhls = let
            hp = pkgs.haskellPackages.extend hp-override;
            hp-override = final: prev: {
              hls-floskell-plugin =
                pkgs.haskell.lib.dontCheck prev.hls-floskell-plugin;
              hls-rename-plugin =
                pkgs.haskell.lib.dontCheck prev.hls-rename-plugin;
              haskell-language-server =
                pkgs.haskell.lib.overrideCabal prev.haskell-language-server
                (drv: {
                  configureFlags = drv.configureFlags ++ [
                    "-f-brittany"
                    "-f-fourmolu"
                    "-f-floskell"
                    "-f-stylishhaskell"
                    "-f-hlint"
                  ];
                });
              ormolu = make-ormolu final;
            };
          in pkgs.haskell-language-server.override {
            haskellPackages = hp;
            dynamic = true;
            supportedGhcVersions = [ ghc-version ];
          };
          myormolu = make-ormolu pkgs.haskellPackages;

          unison-env = pkgs.mkShell {
            packages = with pkgs; [
              mystack
              (haskell.compiler."ghc${ghc-version}".override {
                useLLVM = pkgs.stdenv.isAarch64;
              })
              myormolu
              myhls
              pkg-config
              zlib
            ];
            # workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/11042
            shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
            '';
          };
        in {

          apps.repl = flake-utils.lib.mkApp {
            drv =
              nixpkgs.legacyPackages."${system}".writeShellScriptBin "repl" ''
                confnix=$(mktemp)
                echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
                trap "rm $confnix" EXIT
                nix repl $confnix
              '';
          };

          pkgs = pkgs;

          devShell = unison-env;

          packages = { };

          defaultPackage = self.packages."${system}".unison-env;
        });
      topLevelAttrs = { overlay = final: prev: { }; };
    in systemAttrs // topLevelAttrs;
}
