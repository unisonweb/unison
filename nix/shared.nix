{ compiler ? "ghc844" }:

let
  overlayShared = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                dontHaddock = pkgsOld.haskell.lib.dontHaddock;
                failOnAllWarnings = pkgsOld.haskell.lib.failOnAllWarnings;

                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    easytest =
                      # failOnAllWarnings
                        (haskellPackagesNew.callCabal2nix
                          "easytest"
                          ../yaks/easytest
                          { }
                        );
                    haskeline =
                      failOnAllWarnings
                        (haskellPackagesNew.callCabal2nix
                          "haskeline"
                          ../yaks/haskeline
                          { }
                        );
                    unison-parser-typechecker =
                      dontHaddock
                      # failOnAllWarnings
                        (haskellPackagesNew.callCabal2nix
                          "unison-parser-typechecker"
                          ../parser-typechecker
                          { }
                        );
                  };

              in
                pkgsNew.lib.fold
                  pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  [ (pkgsNew.haskell.lib.packagesFromDirectory { directory = ./.; })

                    extension
                  ];
          }
        );
      };
    };
  };

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src {
    config = {};
    overlays = [ overlayShared ];
  };

in
  rec {
    inherit (pkgs.haskell.packages."${compiler}")
      easytest
      haskeline
      unison-parser-typechecker
    ;

    shell-easytest = (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".easytest).env;
    shell-haskeline = (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".haskeline).env;
    shell-unison-parser-typechecker = (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".unison-parser-typechecker).env;
  }
