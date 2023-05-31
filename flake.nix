{
  description = "A common environment for unison development";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, flake-utils, nixpkgs }:
    let
      ghc-version = "927";
      systemAttrs = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages."${system}".extend self.overlay;
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
              glibcLocales
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
            ghc = pkgs.haskell.compiler."ghc${ghc-version}";
            stack = pkgs.unison-stack;
            devShell = self.devShells."${system}".default;

          };

          defaultPackage = self.packages."${system}".devShell;
        });
      topLevelAttrs = {
        overlay = final: prev: {
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
