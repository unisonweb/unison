{
  description = "A common environment for unison development";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = { self, flake-utils, nixpkgs }:
    let
      ghc-version = "8107";
      systemAttrs = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages."${system}".extend self.overlay;
          ghc-version = "8107";
          ghc = pkgs.haskell.packages."ghc${ghc-version}";
          nativePackages = pkgs.lib.optionals pkgs.stdenv.isDarwin
            (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa ]);

          unison-env = pkgs.mkShell {
            packages = let exports = self.packages."${system}";
            in with pkgs;
            [
              exports.stack
              exports.hls
              exports.ormolu
              exports.ghc
              pkg-config
              zlib
            ] ++ nativePackages;
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

          devShells.default = unison-env;

          packages = {
            hls = pkgs.unison-hls;
            hls-call-hierarchy-plugin = ghc.hls-call-hierarchy-plugin;
            ormolu = pkgs.ormolu;
            ghc = pkgs.haskell.compiler."ghc${ghc-version}".override {
              useLLVM = pkgs.stdenv.isAarch64;
            };
            stack = pkgs.unison-stack;
            devShell = self.devShells."${system}".default;

          };

          defaultPackage = self.packages."${system}".devShell;
        });
      topLevelAttrs = {
        overlay = final: prev: {
          ormolu = prev.haskell.lib.justStaticExecutables
            final.haskell.packages."ghc${ghc-version}".ormolu;
          haskell = with prev.haskell.lib;
            prev.haskell // {
              packages = prev.haskell.packages // {
                "ghc${ghc-version}" = prev.haskell.packages.ghc8107.extend
                  (hfinal: hprev: {
                    mkDerivation = drv:
                      hprev.mkDerivation (drv // {
                        doCheck = false;
                        doHaddock = false;
                        doBenchmark = false;
                        enableLibraryProfiling = false;
                        enableExecutableProfiling = false;
                      });
                    aeson = hfinal.aeson_2_1_1_0;
                    lens-aeson = hfinal.lens-aeson_1_2_2;
                    Cabal = hfinal.Cabal_3_6_3_0;
                    ormolu = hfinal.ormolu_0_5_0_1;
                    ghc-lib-parser = hfinal.ghc-lib-parser_9_2_5_20221107;
                    # avoid deprecated version https://github.com/Avi-D-coder/implicit-hie/issues/50
                    implicit-hie = hfinal.callHackageDirect {
                      pkg = "implicit-hie";
                      ver = "0.1.4.0";
                      sha256 =
                        "15qy9vwm8vbnyv47vh6kd50m09vc4vhqbbrhf8gdifrvlxhad69l";
                    } { };
                    haskell-language-server = let
                      p = prev.haskell.lib.overrideCabal
                        hprev.haskell-language-server (drv: {
                          # undo terrible nixpkgs hacks
                          buildDepends =
                            prev.lib.filter (x: x != hprev.hls-brittany-plugin)
                            drv.buildDepends;
                          configureFlags = drv.configureFlags ++ [
                            "-f-brittany"
                            "-f-fourmolu"
                            "-f-floskell"
                            "-f-stylishhaskell"
                            "-f-hlint"
                          ];
                        });
                    in p.overrideScope (lfinal: lprev: {
                      # undo all of the horrible overrideScope in
                      # nixpkgs configuration files
                      ormolu = hfinal.ormolu;
                      ghc-lib-parser = hfinal.ghc-lib-parser;
                      ghc-lib-parser-ex = hfinal.ghc-lib-parser-ex;
                      ghc-paths = hfinal.ghc-paths;
                      aeson = hfinal.aeson;
                      lsp-types = hfinal.lsp-types;
                      # null out some dependencies that we drop with cabal flags
                      hls-fourmolu-plugin = null;
                      hls-floskell-plugin = null;
                      hls-brittany-plugin = hfinal.hls-brittany-plugin;
                      hls-stylish-haskell-plugin = null;
                      hls-hlint-plugin = null;
                    });
                  });
              };
            };
          unison-hls = final.haskell-language-server.override {
            haskellPackages = final.haskell.packages."ghc${ghc-version}";
            dynamic = true;
            supportedGhcVersions = [ ghc-version ];
          };
          unison-stack = prev.symlinkJoin {
            name = "stack";
            paths = [ final.stack ];
            buildInputs = [ final.makeWrapper ];
            postBuild = let
              flags = [ "--no-nix" "--system-ghc" "--no-install-ghc" ];
              add-flags =
                "--add-flags '${prev.lib.concatStringsSep " " flags}'";
            in ''
              wrapProgram "$out/bin/stack" ${add-flags}
            '';
          };
        };
      };
    in systemAttrs // topLevelAttrs;
}
